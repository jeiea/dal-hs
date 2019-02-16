{-# LANGUAGE CPP #-}

module Win32.System.DLL (module Win32.System.DLL) where

import Win32.Types

foreign import WINDOWS_CCONV "Windows.h GetModuleHandleW"
  c_GetModuleHandle :: LPCTSTR -> IO HMODULE
