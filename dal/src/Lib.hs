{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib (main) where

import Data.Bits
import Foreign
import Foreign.C
import RIO
import RIO.List (delete)
import Text.Printf
import Types
import Win32
import Win32.Kernel32
import Win32.User32 hiding (wParam)

main :: IO ()
main = do
  ctx <- newSomeRef initialContext
  withEmptyLogFuncIfNoConsole $ \lf ->
    let app = App
          { logFunc = lf
          , context = ctx
          }
    in runRIO app run

withEmptyLogFuncIfNoConsole :: (LogFunc -> IO ()) -> IO ()
withEmptyLogFuncIfNoConsole logWriter = do
  outHandle <- getStdHandle STD_OUTPUT_HANDLE
  outMode <- try $ getConsoleMode outHandle
  case outMode of
    Left (W32FailException _) -> logWriter emptyLogger
    Right _ -> do
      lo <- logOptionsHandle stdout False
      withLogFunc lo logWriter

emptyLogger :: LogFunc
emptyLogger = mkLogFunc (\_ _ _ _ -> return ())

initialContext :: KeyProcCtx
initialContext = KeyProcCtx Nothing [] initialHandler

run :: RIO App ()
run = do
  kp <- initKeyProc
  hMod <- getModuleHandle Nothing
  hHook <- setWindowsHookEx WH_KEYBOARD_LL kp hMod 0

  liftIO $ whenM (isJust <$> getConsoleWindow) registerCtrlC

  messagePump
  unhookWindowsHookEx hHook
  logI "quit.\n"

registerCtrlC :: IO ()
registerCtrlC = do
  tid <- getCurrentThreadId
  ctrlC <- pHANDLER_ROUTINE $ interruptHandler tid
  setConsoleCtrlHandler ctrlC True

  -- Enable interrupt handler.
  conIn <- getStdHandle STD_INPUT_HANDLE
  setConsoleMode conIn ENABLE_PROCESSED_INPUT

sendKey :: MonadIO m => WORD -> m UINT
sendKey vk = do
  sc <- mapVirtualKey vk MAPVK_VK_TO_VSC
  sendInput
    [ KINPUT $ KEYBDINPUT vk (fromIntegral sc) 0 0 1
    , KINPUT $ KEYBDINPUT vk (fromIntegral sc) KEYEVENTF_KEYUP 0 1
    ]

sendSpaceKey :: MonadIO m => m UINT
sendSpaceKey = sendKey VK_SPACE

sendKdhs :: MonadIO m => KBDLLHOOKSTRUCT -> m UINT
sendKdhs KBDLLHOOKSTRUCT{..} = sendInput $ KINPUT <$>
  [ kdhs extended 0 1
  , kdhs (KEYEVENTF_KEYUP .|. extended) 0 1
  ] where
    kdhs = KEYBDINPUT (fromIntegral vkCode) (fromIntegral scanCode)
    extended = flags .&. 1

sendBigCtrlKey :: MonadIO m => KBDLLHOOKSTRUCT -> m UINT
sendBigCtrlKey KBDLLHOOKSTRUCT{..} = sendInput $ KINPUT <$>
  [ control 0 0 1
  , moded extended 0 1
  , moded (KEYEVENTF_KEYUP .|. extended) 0 1
  , control KEYEVENTF_KEYUP 0 1
  ] where
    control = KEYBDINPUT VK_CONTROL 29
    moded = KEYBDINPUT (fromIntegral vkCode) (fromIntegral scanCode)
    extended = flags .&. 1

initialHandler :: KeyboardProc
initialHandler msg kdhs = next where
  next = if vkCode kdhs == VK_SPACE && msg == WM_KEYDOWN
    then enterBigCtrlMode msg kdhs
    else return False

enterBigCtrlMode :: KeyboardProc
enterBigCtrlMode _msg kdhs = do
  ref <- view stateRefL
  modifySomeRef ref $ \cx -> cx
    { spacePressTime = Just $ kdhs_time kdhs
    , delegate = bigCtrlHandler
    }
  return True

isModifier :: (Eq a, Num a) => a -> Bool
isModifier vk = vk `elem`
  [ VK_MENU
  , VK_SHIFT, VK_LSHIFT, VK_RSHIFT
  , VK_CONTROL, VK_LCONTROL, VK_RCONTROL ]

bigCtrlHandler :: KeyboardProc
bigCtrlHandler msg kdhs =
  if vk == VK_SPACE
    then if msg == WM_KEYUP
      then exitBigCtrlMode msg kdhs
      else return True
    else do
      ref <- view stateRefL
      keysAfterSpace <- kdhsAfterSpace <$> readSomeRef ref
      if msg == WM_KEYDOWN
        then do -- key pressed during pressing space key
          if isModifier vk then return False else do
            modifySomeRef ref $ \ctx -> ctx
              { kdhsAfterSpace = normalized : keysAfterSpace
              }
            return True
        else do -- key released during pressing space key
          if normalized `elem` keysAfterSpace
          then do
            ctx <- readSomeRef ref
            writeSomeRef ref ctx
              { kdhsAfterSpace = delete normalized keysAfterSpace
              , spacePressTime = Nothing
              }
            _ <- sendBigCtrlKey kdhs
            return True
          else return False
  where
    vk = vkCode kdhs
    normalized = kdhs {flags = 0, kdhs_time = 0, dwExtraInfo = 0}

exitBigCtrlMode :: KeyboardProc
exitBigCtrlMode _msg kdhs = do
  ref <- view stateRefL
  ctx <- readSomeRef ref
  sequence_ $ sendKdhs <$> reverse (kdhsAfterSpace ctx)
  modifySomeRef ref $ \cx -> cx
    { delegate = initialHandler
    , spacePressTime = Nothing
    , kdhsAfterSpace = []
    }
  let shortPressed = maybe False (kdhs_time kdhs - 500 <) (spacePressTime ctx)
  _ <- when' shortPressed sendSpaceKey
  return True

initKeyProc :: RIO App HOOKPROC
initKeyProc = do
  ari <- askRunInIO
  llkp <- liftIO $ lowLevelKeyboardProc $ kp ari
  return (hOOKPROC llkp)
  where
    kp ari c w l = ari (keyProc c w l)

logI :: HasLogFunc env => String -> RIO env ()
logI = logInfo . fromString

-- type LowLevelKeyboardProc = LONG -> WPARAM -> Ptr KBDLLHOOKSTRUCT -> IO LRESULT
keyProc
  :: LONG -> WPARAM -> Ptr KBDLLHOOKSTRUCT
  -> RIO App LRESULT
keyProc code w l = do
  kb <- liftIO $ peek l
  name <- bracketWin32 $ getKeyNameText (fromIntegral $ kdhsToLParam kb)
  logI $ printf "code:%d, w:%3d, vk:%3d, flg:%4X, ti:%8d, ex:%d, %-10s %s"
    (toInteger code) (toInteger w) (toInteger $ vkCode kb) (toInteger $ flags kb)
    (toInteger $ kdhs_time kb) (toInteger $ dwExtraInfo kb)
    (if w == WM_KEYDOWN then "WM_KEYDOWN" else "WM_KEYUP" :: Text)
    name

  ref <- view stateRefL
  if dwExtraInfo kb /= 0 then liftIO toss else do
    KeyProcCtx{delegate = dele} <- readSomeRef ref
    hooked <- dele w kb
    return $ if hooked then 1 else 0
  where
    toss :: IO LRESULT
    toss = callNextHookEx code w (ptrToCPtr l)

kdhsToLParam :: KBDLLHOOKSTRUCT -> DWORD
kdhsToLParam kb = scanCode kb `shiftL` 16 .|. (flags kb .&. 1) `shiftL` 24

ptrToCPtr :: Ptr a -> CIntPtr
ptrToCPtr = CIntPtr . fromInteger . toInteger . ptrToIntPtr

when' :: Applicative m => Bool -> m a -> m ()
when' p act = if p then act *> pure () else pure ()

messageLoop :: (HasLogFunc env) => LPMSG -> RIO env ()
messageLoop wm = do
  r <- getMessage wm Nothing (0, 0)
  -- liftIO . printf $ show wm
  when r $ do
    _ <- dispatchMessage wm
    messageLoop wm

messagePump :: (HasLogFunc env) => RIO env ()
messagePump = do
  ari <- askRunInIO
  let loop = ari . messageLoop
  liftIO $ alloca loop

interruptHandler :: DWORD -> DWORD -> IO Bool
interruptHandler mainThreadId _dw = do
  postThreadMessage mainThreadId WM_QUIT 0 0
  return True
