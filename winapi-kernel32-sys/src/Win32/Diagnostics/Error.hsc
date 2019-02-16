{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}

module Win32.Diagnostics.Error (module Win32.Diagnostics.Error) where

import Win32.Types
import Data.Bits
import Foreign

foreign import WINDOWS_CCONV "Windows.h GetLastError"
  c_GetLastError :: IO DWORD

pattern FORMAT_MESSAGE_ALLOCATE_BUFFER = 0x00000100
pattern FORMAT_MESSAGE_FROM_SYSTEM     = 0x00001000

foreign import WINDOWS_CCONV "Windows.h FormatMessageW"
  c_FormatMessageW
    :: DWORD -> LPCVOID -> DWORD -> DWORD
    -> LPTSTR -> DWORD -> DWORD -> IO DWORD
