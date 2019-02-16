{-# LANGUAGE CPP #-}

module Win32.System.Thread (module Win32.System.Thread) where

import Win32.Types

foreign import WINDOWS_CCONV "Windows.h GetCurrentThreadId"
  c_GetCurrentThreadId :: IO DWORD
