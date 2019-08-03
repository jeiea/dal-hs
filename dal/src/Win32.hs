{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Win32 where

import Foreign
import Foreign.C
import RIO
import qualified Win32.Kernel32 as W
import qualified Win32.User32 as W

-- common 
-- Custom Exception

newtype Win32FailException = W32FailException Text
   deriving (Show, Typeable)

w32Fail :: MonadThrow m => Text -> m a
w32Fail = throwM . W32FailException

instance Exception Win32FailException

-- kernel32

-- Console
getConsoleMode :: (MonadUnliftIO m, MonadThrow m) => W.HANDLE -> m W.DWORD
getConsoleMode conHandle = withRunInIO $ \runner ->
  alloca $ \p -> runner $ do
    success <- liftIO $ W.c_GetConsoleMode conHandle p
    if success
    then liftIO $ peek p
    else w32Fail "getConsoleMode failed."

setConsoleMode :: (MonadIO m, MonadThrow m)
  => W.HANDLE -> W.ConsoleMode -> m ()
setConsoleMode conHandle (W.ConsoleMode flag) = do
  res <- liftIO $ W.c_SetConsoleMode conHandle flag
  if res then return () else w32Fail "setConsoleMode Failed."

getStdHandle :: (MonadIO m, MonadThrow m)
  => W.StdHandleFlag -> m W.HANDLE
getStdHandle (W.StdHandleFlag flag) = do
  hCon <- liftIO $ W.c_GetStdHandle flag
  if hCon /= W.INVALID_HANDLE_VALUE
  then return hCon
  else w32Fail "getStdHandle Failed."

setConsoleCtrlHandler :: (MonadIO m, MonadThrow m)
  => W.PHANDLER_ROUTINE -> W.BOOL -> m ()
setConsoleCtrlHandler handler add = do
  res <- liftIO $ W.c_SetConsoleCtrlHandler handler add
  if res then return () else w32Fail "setConsoleCtrlHandler Failed."

addConsoleAlias :: (MonadIO m, MonadThrow m)
  => W.LPCTSTR -> W.LPCTSTR -> W.LPCTSTR -> m ()
addConsoleAlias source target exe = do
  res <- liftIO $ W.c_AddConsoleAlias source target exe
  if res then return () else w32Fail "addConsoleAlias Failed."

allocConsole :: (MonadIO m, MonadThrow m) => m ()
allocConsole = do
  res <- liftIO W.c_AllocConsole
  if res then return () else w32Fail "allocConsole Failed."

getConsoleWindow :: MonadIO m => m (Maybe W.HWND)
getConsoleWindow = do
  hCon <- liftIO W.c_GetConsoleWindow
  return $ if hCon == W.nullHANDLE then Nothing else Just hCon

-- Error

getLastError :: MonadIO m => m W.DWORD
getLastError = liftIO W.c_GetLastError

formatMessage :: MonadIO m => W.DWORD -> m String
formatMessage err = liftIO $ alloca $ \p -> do
  let flags = W.FORMAT_MESSAGE_ALLOCATE_BUFFER .|. W.FORMAT_MESSAGE_FROM_SYSTEM
  written <- W.c_FormatMessageW flags 0 err 0 (castPtr p) 0 0
  ps <- peek p
  if written == 0 then return "" else peekCWString (castPtr ps)

-- DLL

getModuleHandle :: MonadIO m => Maybe String -> m W.HMODULE
getModuleHandle name = liftIO $ withAlloc getMod where
  withAlloc f = case name of
    Just x -> withCWString x f
    Nothing -> f nullPtr
  getMod cws = do
    hMod <- liftIO $ W.c_GetModuleHandle cws
    if hMod /= nullPtr
    then return hMod
    else w32Fail "getModuleHandle Failed."

-- Thread

getCurrentThreadId :: MonadIO m => m W.DWORD
getCurrentThreadId = liftIO W.c_GetCurrentThreadId

-- bracket

bracketWin32 :: MonadUnliftIO m => m a -> m a
bracketWin32 act = do
  nam <- try act
  case nam of
    Right res -> return res
    Left e@(W32FailException _) -> liftIO $ do
      hPutBuilder stderr . getUtf8Builder $ displayShow e
      err <- getLastError
      msg <- formatMessage err
      hPutBuilder stderr . getUtf8Builder $ displayShow msg
      throwM e

-- user32

messageBox :: MonadIO m => String -> String -> m Int
messageBox text caption = liftIO .
  withCWString text $ \cText-> do
    withCWString caption $ \cCaption -> do
      res <- W.c_MessageBoxW W.nullHANDLE cText cCaption 0
      return $ fromIntegral res

-- Hook

setWindowsHookEx :: (MonadIO m, MonadThrow m)
  => W.WHKind -> W.HOOKPROC -> W.HINSTANCE -> W.DWORD
  -> m W.HHOOK
setWindowsHookEx (W.WHKind idHook) lpfn hMod dwThreadId = do
  hHook <- liftIO $
    W.c_SetWindowsHookEx idHook lpfn hMod dwThreadId
  if hHook /= W.nullHANDLE
  then return hHook
  else w32Fail "setWindowsHookEx Failed."

unhookWindowsHookEx :: (MonadIO m, MonadThrow m)
  => W.HHOOK -> m ()
unhookWindowsHookEx hHook = do
  success <- liftIO $ W.c_UnhookWindowsHookEx hHook
  if success
  then return ()
  else w32Fail "UnhookWindowsHookEx Failed."

-- Message

getMessage :: (MonadIO m, MonadThrow m)
  => W.LPMSG -> Maybe W.HWND -> (W.UINT, W.UINT)
  -> m Bool
getMessage msg mbHWnd (fMin, fMax) = do
  res <- liftIO $ W.c_GetMessage msg (W.maybeHANDLE mbHWnd) fMin fMax
  if res >= 0
  then return $ res > 0
  else w32Fail "getMessage Failed."

peekMessage :: MonadIO m
  => W.LPMSG -> Maybe W.HWND -> (W.UINT, W.UINT) -> W.UINT
  -> m Bool
peekMessage msg mbHWnd (fMin, fMax) remove = do
  res <- liftIO $ W.c_PeekMessage msg (W.maybeHANDLE mbHWnd) fMin fMax remove
  return $ res /= 0

translateMessage :: (MonadIO m) => W.LPMSG -> m Bool
translateMessage = liftIO . W.c_TranslateMessage

dispatchMessage :: (MonadIO m) => W.LPMSG -> m W.LONG
dispatchMessage = liftIO . W.c_DispatchMessage

sendMessage :: MonadIO m
  => W.HWND -> W.UINT -> W.WPARAM -> W.LPARAM
  -> m W.LRESULT
sendMessage hWnd wm w l = liftIO $ W.c_SendMessage hWnd wm w l

postQuitMessage :: MonadIO m => W.DWORD -> m ()
postQuitMessage = liftIO . W.c_PostQuitMessage

postThreadMessage :: (MonadIO m, MonadThrow m)
  => W.DWORD -> W.UINT -> W.WPARAM -> W.LPARAM
  -> m ()
postThreadMessage tId msg w l = do
  success <- liftIO $ W.c_PostThreadMessage tId msg w l
  if success
  then return ()
  else w32Fail "postThreadMessage Failed."

-- Window

-- TODO: Catch IOException
registerClass :: MonadIO m => W.WNDCLASS -> m W.ATOM
registerClass wc = liftIO $ with wc $ \p -> do
  res <- liftIO $ W.c_RegisterClass p
  if res /= 0
  then return res
  else w32Fail "registerClass Failed."

maybePos :: Maybe W.Pos -> W.Pos
maybePos = fromMaybe W.cW_USEDEFAULT

createWindow
  :: W.ClassName -> W.LPCTSTR -> W.WindowStyle
  -> Maybe W.Pos -> Maybe W.Pos -> Maybe W.Pos -> Maybe W.Pos
  -> Maybe W.HWND -> Maybe W.HMENU -> W.HINSTANCE -> W.LPVOID
  -> IO W.HWND
createWindow = createWindowEx 0

createWindowEx :: (MonadIO m, MonadThrow m)
  => W.WindowStyle -> W.ClassName -> W.LPCTSTR -> W.WindowStyle
  -> Maybe W.Pos -> Maybe W.Pos -> Maybe W.Pos -> Maybe W.Pos
  -> Maybe W.HWND -> Maybe W.HMENU -> W.HINSTANCE -> W.LPVOID
  -> m W.HWND
createWindowEx
  exStyle cName wName wStyle
  mbX mbY mbW mbH mbParent mbMenu inst lpParam = do
  -- Freeing the title/window name has been reported
  -- to cause a crash, so let's not do it.
  -- withTString wName $ \ W.c_wname -> do
  -- W.c_wname <- liftIO $ newTString wName
  hWnd <- liftIO $ W.c_CreateWindowEx exStyle cName wName wStyle
    (maybePos mbX) (maybePos mbY) (maybePos mbW) (maybePos mbH)
    (W.maybeHANDLE mbParent) (W.maybeHANDLE mbMenu) inst lpParam
  if hWnd /= W.nullHANDLE
  then return hWnd
  else w32Fail "createWindowEx Failed."

defWindowProc :: MonadIO m
  => Maybe W.HWND -> W.UINT -> W.WPARAM -> W.LPARAM
  -> m W.LRESULT
defWindowProc mbHWnd msg w =
  liftIO . W.c_DefWindowProc (W.maybeHANDLE mbHWnd) msg w

--

sendInput :: (MonadIO m, Foldable f) => f W.INPUT -> m W.UINT
sendInput inputs = do
  liftIO $ withArray (toList inputs) $ \p ->
    W.c_SendInput' len p
  where len = fromInteger . toInteger $ length inputs

getKeyNameText :: MonadIO m => W.LONG -> m String
getKeyNameText lParam =
  liftIO $ allocaBytes (cchSize * 2) $ \p -> do
    written <- W.c_GetKeyNameTextW lParam p (fromIntegral cchSize)
    if written == 0
      then w32Fail "getKeyNameText failed."
      else peekCWString p
  where cchSize = 128

getKeyState :: MonadIO m => CInt -> m W.SHORT
getKeyState = liftIO . W.c_GetKeyState

mapVirtualKey :: (MonadIO m, Integral a) => a -> W.UINT -> m W.UINT
mapVirtualKey uCode uMapType =
  liftIO $ W.c_MapVirtualKeyW (fromIntegral uCode) uMapType
