{-# LANGUAGE CPP #-}

module Win32.User32
    ( module Win32.User32
    , module Win32.WindowMessage
    , module Win32.GDI
    , module Win32.Input
    , module Win32.Types
    ) where

import Win32.GDI
import Win32.Input
import Win32.Types
import Win32.WindowMessage
import Foreign.C

foreign import WINDOWS_CCONV "WinUser.h MessageBoxW"
  c_MessageBoxW
    :: HWND    -- hWnd
    -> LPCWSTR -- lpText
    -> LPCWSTR -- lpCaption
    -> UINT    -- uType
    -> IO CInt