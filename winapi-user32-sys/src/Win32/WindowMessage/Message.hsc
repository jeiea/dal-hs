{-# LANGUAGE CPP #-}

module Win32.WindowMessage.Message (
    module Win32.WindowMessage.Message
  , module Win32.WindowMessage.Message.Constants
  ) where

import Control.Monad (ap)
import Foreign
import Foreign.C
import Foreign.Ptr
import Win32.Types
import Win32.WindowMessage.Message.Constants
import Win32.GDI

#include <Windows.h>

----------------------------------------------------------------
-- MSGs and event loops
--
-- Note that the following functions have to be reentrant:
--
--   DispatchMessage
--   SendMessage
--   UpdateWindow   (I think)
--   RedrawWindow   (I think)
--
-- The following dont have to be reentrant (according to documentation)
-- (but I think I saw re-entry)
--
--   GetMessage
--   PeekMessage
--   TranslateMessage
--
-- For Hugs (and possibly NHC too?) this is no big deal.
-- For GHC, you have to use casm_GC instead of casm.
-- (It might be simpler to just put all this code in another
-- file and build it with the appropriate command line option...)
----------------------------------------------------------------

data MSG = MSG {
    hwnd :: HWND
  , message :: UINT
  , wParam :: WPARAM
  , lParam :: LPARAM
  , time :: DWORD
  , pt :: POINT
  } deriving Show

instance Storable MSG where
  sizeOf    _ = #{size MSG}
  alignment   = sizeOf

  poke p msg = do
    #{poke MSG, hwnd} p $ hwnd msg
    #{poke MSG, message} p $ message msg
    #{poke MSG, wParam} p $ wParam msg
    #{poke MSG, lParam} p $ lParam msg
    #{poke MSG, time} p $ time msg
    #{poke MSG, pt} p $ pt msg

  peek p = return MSG
    `ap` #{peek MSG, hwnd} p
    `ap` #{peek MSG, message} p
    `ap` #{peek MSG, wParam} p
    `ap` #{peek MSG, lParam} p
    `ap` #{peek MSG, time} p
    `ap` #{peek MSG, pt} p

type LPMSG   = Addr

-- A NULL window requests messages for any window belonging to this thread.
-- a "success" value of 0 indicates that WM_QUIT was received

foreign import WINDOWS_CCONV safe "windows.h GetMessageW"
  c_GetMessage :: LPMSG -> HWND -> UINT -> UINT -> IO LONG

-- A NULL window requests messages for any window belonging to this thread.
-- Arguably the code block shouldn't be a 'safe' one, but it shouldn't really
-- hurt.

foreign import WINDOWS_CCONV "windows.h PeekMessageW"
  c_PeekMessage :: LPMSG -> HWND -> UINT -> UINT -> UINT -> IO LONG

-- Note: you're not supposed to call this if you're using accelerators
foreign import WINDOWS_CCONV "windows.h TranslateMessage"
  c_TranslateMessage :: LPMSG -> IO BOOL

foreign import WINDOWS_CCONV "windows.h DispatchMessageW"
  c_DispatchMessage :: LPMSG -> IO LONG

foreign import WINDOWS_CCONV "windows.h SendMessageW"
  c_SendMessage :: HWND -> UINT -> WPARAM -> LPARAM -> IO LRESULT

foreign import WINDOWS_CCONV "windows.h PostQuitMessage"
  c_PostQuitMessage :: DWORD -> IO ()

foreign import WINDOWS_CCONV "windows.h PostThreadMessageW"
  c_PostThreadMessage :: DWORD -> UINT -> WPARAM -> LPARAM -> IO BOOL
