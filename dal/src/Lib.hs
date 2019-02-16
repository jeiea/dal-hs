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
import Win32
import Win32.Kernel32
import Win32.User32 hiding (wParam)

main :: IO ()
main = do
  lo <- logOptionsHandle stdout False
  withLogFunc lo $ \lf -> do
    runRIO lf run

run :: HasLogFunc env => RIO env ()
run = do
  kp <- liftIO keyProcInit
  hMod <- getModuleHandle Nothing
  hHook <- setWindowsHookEx WH_KEYBOARD_LL kp hMod 0

  liftIO $ whenM (isJust <$> getConsoleWindow) registerCtrlC

  messagePump
  unhookWindowsHookEx hHook

registerCtrlC :: IO ()
registerCtrlC = do
  -- Register Ctrl+C interrupt handler for posting WM_QUIT.
  -- Maybe it's possible to use InterruptibleFFI extension,
  -- but let it be unless there is a reason.
  tid <- getCurrentThreadId
  ctrlC <- pHANDLER_ROUTINE $ interruptHandler tid
  setConsoleCtrlHandler ctrlC True

  -- Enable interrupt handler.
  conIn <- getStdHandle STD_INPUT_HANDLE
  setConsoleMode conIn ENABLE_PROCESSED_INPUT

sendKey :: WORD -> IO UINT
sendKey vk = do
  sc <- mapVirtualKey vk MAPVK_VK_TO_VSC
  sendInput
    [ KINPUT $ KEYBDINPUT vk (fromIntegral sc) 0 0 1
    , KINPUT $ KEYBDINPUT vk (fromIntegral sc) KEYEVENTF_KEYUP 0 1
    ]

sendSpaceKey :: IO UINT
sendSpaceKey = sendKey VK_SPACE

sendKdhs :: KBDLLHOOKSTRUCT -> IO UINT
sendKdhs KBDLLHOOKSTRUCT{..} = sendInput $ KINPUT <$>
  [ kdhs extended 0 1
  , kdhs (KEYEVENTF_KEYUP .|. extended) 0 1
  ] where
    kdhs = KEYBDINPUT (fromIntegral vkCode) (fromIntegral scanCode)
    extended = flags .&. 1

sendBigCtrlKey :: KBDLLHOOKSTRUCT -> IO UINT
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
initialHandler ctx@KeyProcCtx{..} = next where
  next = if vkCode kb == VK_SPACE && wParam == WM_KEYDOWN
    then enterBigCtrlMode ctx
    else toss

enterBigCtrlMode :: KeyboardProc
enterBigCtrlMode KeyProcCtx{..} = do
  modifyIORef self $ \cx -> cx
    { spacePressTime = Just $ kdhs_time kb
    , delegate = bigCtrlHandler }
  return 1

isModifier :: (Eq a, Num a) => a -> Bool
isModifier vk = vk `elem`
  [ VK_MENU
  , VK_SHIFT, VK_LSHIFT, VK_RSHIFT
  , VK_CONTROL, VK_LCONTROL, VK_RCONTROL ]

bigCtrlHandler :: KeyboardProc
bigCtrlHandler ctx@KeyProcCtx{..} =
  if vk == VK_SPACE
    then if wParam == WM_KEYUP
      then exitBigCtrlMode ctx
      else return 1
    else if wParam == WM_KEYDOWN
      then do -- key pressed during pressing space key
        if isModifier vk then toss else do
          let prepended = normalized : kdhsAfterSpace
          writeIORef self ctx {kdhsAfterSpace = prepended}
          return 1
      else do -- key released during pressing space key
        if normalized `elem` kdhsAfterSpace
        then do
          writeIORef self ctx
            { kdhsAfterSpace = delete normalized $ toList kdhsAfterSpace
            , spacePressTime = Nothing }
          _ <- sendBigCtrlKey kb
          return 1
        else toss
  where
    vk = vkCode kb
    normalized = kb {flags = 0, kdhs_time = 0, dwExtraInfo = 0}

exitBigCtrlMode :: KeyboardProc
exitBigCtrlMode KeyProcCtx{..} = do
  _ <- sequence $ sendKdhs <$> reverse kdhsAfterSpace
  modifyIORef self $ \cx -> cx
    { delegate = initialHandler
    , spacePressTime = Nothing
    , kdhsAfterSpace = [] }
  let shortPressed = maybe False (kdhs_time kb - 500 <) spacePressTime
  when' shortPressed sendSpaceKey
  return 1

type KeyboardProc = KeyProcCtx -> IO LRESULT

data KeyProcCtx = KeyProcCtx
  { self :: IORef KeyProcCtx
  , spacePressTime :: Maybe DWORD
  , kdhsAfterSpace :: [KBDLLHOOKSTRUCT]
  , delegate :: KeyboardProc
  , wParam :: WPARAM
  , kb :: KBDLLHOOKSTRUCT
  , toss :: IO LRESULT
  }

keyProcInit :: IO HOOKPROC
keyProcInit = do
  let und = error "not initialized"
  ctx <- newIORef $ KeyProcCtx und Nothing [] initialHandler 0 und und
  modifyIORef ctx (\cx -> cx {self = ctx})
  hOOKPROC <$> lowLevelKeyboardProc (keyProc ctx)

-- LowLevelKeyboardProc = LONG -> WPARAM -> Ptr KBDLLHOOKSTRUCT -> IO LRESULT
keyProc :: IORef KeyProcCtx -> LowLevelKeyboardProc
keyProc keyProcCtx code w l = do
  kb <- peek l
  printf "code:%d, w:%3d, vk:%3d, fla:%4X, ti:%8d, ex:%d, %-10s "
    (toInteger code) (toInteger w) (toInteger $ vkCode kb) (toInteger $ flags kb)
    (toInteger $ kdhs_time kb) (toInteger $ dwExtraInfo kb)
    (if w == WM_KEYDOWN then "WM_KEYDOWN" else "WM_KEYUP" :: Text)
  name <- bracketWin32 $ getKeyNameText (fromIntegral $ kdhsToLParam kb)
  printf "%s\n" name

  if dwExtraInfo kb /= 0 then toss else do
    modifyIORef keyProcCtx (\cx -> cx {wParam = w, kb = kb, toss = toss})
    ctx@KeyProcCtx{delegate = dele} <- readIORef keyProcCtx
    dele ctx
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
  when r $ do
    logDebug $ displayShow wm <> display ("\r\n" :: Text)
    --translateMessage wm
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
