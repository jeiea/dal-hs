{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Win32 where

import Foreign
import RIO
import Win32.Kernel32
import Win32.User32

-- common 
-- Custom Exception

newtype Win32FailException = W32FailException Text
   deriving (Show, Typeable)

w32Fail :: MonadThrow m => Text -> m a
w32Fail = throwM . W32FailException

instance Exception Win32FailException

-- kernel32

-- Console
setConsoleMode :: (MonadIO m, MonadThrow m)
  => HANDLE -> ConsoleMode -> m ()
setConsoleMode handle (ConsoleMode flag) = do
  res <- liftIO $ c_SetConsoleMode handle flag
  if res then return () else w32Fail "setConsoleMode Failed."

getStdHandle :: (MonadIO m, MonadThrow m)
  => StdHandleFlag -> m HANDLE
getStdHandle (StdHandleFlag flag) = do
  hCon <- liftIO $ c_GetStdHandle flag
  if hCon /= INVALID_HANDLE_VALUE
  then return hCon
  else w32Fail "getStdHandle Failed."

setConsoleCtrlHandler :: (MonadIO m, MonadThrow m)
  => PHANDLER_ROUTINE -> BOOL -> m ()
setConsoleCtrlHandler handler add = do
  res <- liftIO $ c_SetConsoleCtrlHandler handler add
  if res then return () else w32Fail "setConsoleCtrlHandler Failed."

addConsoleAlias :: (MonadIO m, MonadThrow m)
  => LPCTSTR -> LPCTSTR -> LPCTSTR -> m ()
addConsoleAlias source target exe = do
  res <- liftIO $ c_AddConsoleAlias source target exe
  if res then return () else w32Fail "addConsoleAlias Failed."

allocConsole :: (MonadIO m, MonadThrow m) => m ()
allocConsole = do
  res <- liftIO c_AllocConsole
  if res then return () else w32Fail "allocConsole Failed."

getConsoleWindow :: MonadIO m => m (Maybe HWND)
getConsoleWindow = do
  hCon <- liftIO c_GetConsoleWindow
  return $ if hCon == nullHANDLE then Nothing else Just hCon

-- Error

getLastError :: MonadIO m => m DWORD
getLastError = liftIO c_GetLastError

formatMessage :: MonadIO m => DWORD -> m String
formatMessage err = do
  liftIO $ alloca $ \p -> do
    let flags = FORMAT_MESSAGE_ALLOCATE_BUFFER .|. FORMAT_MESSAGE_FROM_SYSTEM
    written <- c_FormatMessageW flags 0 err 0 (castPtr p) 0 0
    ps <- peek p
    if written == 0 then return "" else peekCWString (castPtr ps)

-- DLL

getModuleHandle :: MonadIO m => Maybe String -> m HMODULE
getModuleHandle name = liftIO $ withName name real where
  withName name f = case name of
    Just x -> withCWString x f
    Nothing -> f nullPtr
  real cw = do
    hMod <- liftIO $ c_GetModuleHandle cw
    if hMod /= nullPtr
    then return hMod
    else w32Fail "getModuleHandle Failed."

-- Thread

getCurrentThreadId :: (MonadIO m) => m DWORD
getCurrentThreadId = liftIO c_GetCurrentThreadId

-- bracket

bracketWin32 :: (MonadIO m, MonadUnliftIO m) => m a -> m a
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

-- Hook

setWindowsHookEx :: (MonadIO m, MonadThrow m) =>
  WHKind -> HOOKPROC -> HINSTANCE -> DWORD -> m HHOOK
setWindowsHookEx (WHKind idHook) lpfn hMod dwThreadId = do
  hHook <- liftIO $
    c_SetWindowsHookEx idHook lpfn hMod dwThreadId
  if hHook /= nullHANDLE
  then return hHook
  else w32Fail "setWindowsHookEx Failed."

unhookWindowsHookEx :: (MonadIO m, MonadThrow m) => HHOOK -> m ()
unhookWindowsHookEx hHook = do
  success <- liftIO $ c_UnhookWindowsHookEx hHook
  if success
  then return ()
  else w32Fail "UnhookWindowsHookEx Failed."

-- Message

getMessage :: (MonadIO m, MonadThrow m) => LPMSG -> Maybe HWND -> (UINT, UINT) -> m Bool
getMessage msg mbHWnd (fMin, fMax) = do
  res <- liftIO $ c_GetMessage msg (maybeHANDLE mbHWnd) fMin fMax
  if res >= 0
  then return $ res > 0
  else w32Fail "getMessage Failed."

peekMessage :: MonadIO m
  => LPMSG -> Maybe HWND -> (UINT, UINT) -> UINT -> m Bool
peekMessage msg mbHWnd (fMin, fMax) remove = do
  res <- liftIO $ c_PeekMessage msg (maybeHANDLE mbHWnd) fMin fMax remove
  return $ res /= 0

translateMessage :: (MonadIO m) => LPMSG -> m Bool
translateMessage = liftIO . c_TranslateMessage

dispatchMessage :: (MonadIO m) => LPMSG -> m LONG
dispatchMessage = liftIO . c_DispatchMessage

sendMessage :: (MonadIO m) => HWND -> UINT -> WPARAM -> LPARAM -> m LRESULT
sendMessage hWnd wm w l = liftIO $ c_SendMessage hWnd wm w l

postQuitMessage :: (MonadIO m) => DWORD -> m ()
postQuitMessage = liftIO . c_PostQuitMessage

postThreadMessage :: (MonadIO m, MonadThrow m)
  => DWORD -> UINT -> WPARAM -> LPARAM -> m ()
postThreadMessage tId msg w l = do
  success <- liftIO $ c_PostThreadMessage tId msg w l
  if success
  then return ()
  else w32Fail "postThreadMessage Failed."

-- Window

-- TODO: Catch IOException
registerClass :: MonadIO m => WNDCLASS -> m ATOM
registerClass wc = liftIO $ with wc $ \p -> do
  res <- liftIO $ c_RegisterClass p
  if res /= 0
  then return res
  else w32Fail "registerClass Failed."

maybePos :: Maybe Pos -> Pos
maybePos = fromMaybe cW_USEDEFAULT

createWindow
  :: ClassName -> LPCTSTR -> WindowStyle ->
     Maybe Pos -> Maybe Pos -> Maybe Pos -> Maybe Pos ->
     Maybe HWND -> Maybe HMENU -> HINSTANCE -> LPVOID ->
     IO HWND
createWindow = createWindowEx 0

createWindowEx :: (MonadIO m, MonadThrow m)
  => WindowStyle -> ClassName -> LPCTSTR -> WindowStyle
  -> Maybe Pos -> Maybe Pos -> Maybe Pos -> Maybe Pos
  -> Maybe HWND -> Maybe HMENU -> HINSTANCE -> LPVOID
  -> m HWND
createWindowEx exStyle cName wName wStyle mbX mbY mbW mbH mbParent mbMenu inst lpParam = do
  -- Freeing the title/window name has been reported
  -- to cause a crash, so let's not do it.
  -- withTString wName $ \ c_wname -> do
  -- c_wname <- liftIO $ newTString wName
  hWnd <- liftIO $ c_CreateWindowEx exStyle cName wName wStyle
    (maybePos mbX) (maybePos mbY) (maybePos mbW) (maybePos mbH)
    (maybeHANDLE mbParent) (maybeHANDLE mbMenu) inst lpParam
  if hWnd /= nullHANDLE
  then return hWnd
  else w32Fail "createWindowEx Failed."

defWindowProc :: (MonadIO m) => Maybe HWND -> UINT -> WPARAM -> LPARAM -> m LRESULT
defWindowProc mbHWnd msg w = liftIO . c_DefWindowProc (maybeHANDLE mbHWnd) msg w

--

sendInput :: (MonadIO m, Foldable f) => f INPUT -> m UINT
sendInput inputs = do
  liftIO $ withArray (toList inputs) $ \p ->
    c_SendInput' len p
  where len = fromInteger . toInteger $ length inputs

getKeyNameText :: MonadIO m => LONG -> m String
getKeyNameText lParam =
  liftIO $ allocaBytes (cchSize * 2) $ \p -> do
    written <- c_GetKeyNameTextW lParam p (fromIntegral cchSize)
    if written == 0
      then w32Fail "getKeyNameText failed."
      else peekCWString p
  where cchSize = 128

getKeyState :: MonadIO m => CInt -> m SHORT
getKeyState = liftIO . c_GetKeyState

mapVirtualKey :: MonadIO m => Integral a => a -> UINT -> m UINT
mapVirtualKey uCode uMapType = liftIO $ c_MapVirtualKeyW (fromIntegral uCode) uMapType
