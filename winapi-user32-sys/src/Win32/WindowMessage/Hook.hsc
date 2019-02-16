{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Win32.WindowMessage.Hook (module Win32.WindowMessage.Hook) where

import Control.Monad (ap)
import Foreign
import Foreign.C
import Foreign.Ptr
import Win32.Types

##include "windows_cconv.h"

#include <Windows.h>

newtype WHKind = WHKind LONG deriving (Eq, Ord, Show)

pattern WH_CALLWNDPROC     = WHKind (#{const WH_CALLWNDPROC})
pattern WH_CALLWNDPROCRET  = WHKind (#{const WH_CALLWNDPROCRET})
pattern WH_CBT             = WHKind (#{const WH_CBT})
pattern WH_DEBUG           = WHKind (#{const WH_DEBUG})
pattern WH_FOREGROUNDIDLE  = WHKind (#{const WH_FOREGROUNDIDLE})
pattern WH_GETMESSAGE      = WHKind (#{const WH_GETMESSAGE})
pattern WH_JOURNALPLAYBACK = WHKind (#{const WH_JOURNALPLAYBACK})
pattern WH_JOURNALRECORD   = WHKind (#{const WH_JOURNALRECORD})
pattern WH_KEYBOARD        = WHKind (#{const WH_KEYBOARD})
pattern WH_KEYBOARD_LL     = WHKind (#{const WH_KEYBOARD_LL})
pattern WH_MOUSE           = WHKind (#{const WH_MOUSE})
pattern WH_MOUSE_LL        = WHKind (#{const WH_MOUSE_LL})
pattern WH_MSGFILTER       = WHKind (#{const WH_MSGFILTER})
pattern WH_SHELL           = WHKind (#{const WH_SHELL})
pattern WH_SYSMSGFILTER    = WHKind (#{const WH_SYSMSGFILTER})

-- TODO: implement type-safe
newtype HOOKPROC = HOOKPROC (Ptr ())

hOOKPROC :: FunPtr a -> HOOKPROC
hOOKPROC = HOOKPROC . castFunPtrToPtr

type HHOOK = HANDLE

foreign import WINDOWS_CCONV "windows.h SetWindowsHookExW"
  c_SetWindowsHookEx ::
    LONG -> HOOKPROC -> HINSTANCE -> DWORD -> IO HHOOK
--  idHook  lpfn        hMod         dwThreadId  hHook

foreign import WINDOWS_CCONV "windows.h UnhookWindowsHookEx"
  c_UnhookWindowsHookEx :: HHOOK -> IO BOOL

type LowLevelKeyboardProc = LONG -> WPARAM -> Ptr KBDLLHOOKSTRUCT -> IO LRESULT

foreign import WINDOWS_CCONV "wrapper"
  lowLevelKeyboardProc :: LowLevelKeyboardProc -> IO (FunPtr LowLevelKeyboardProc)

-- type LLKeyboardProc = WPARAM -> KBDLLHOOKSTRUCT -> IO LRESULT

data KBDLLHOOKSTRUCT = KBDLLHOOKSTRUCT
  { vkCode      :: DWORD
  , scanCode    :: DWORD
  , flags       :: DWORD
  , kdhs_time   :: DWORD
  , dwExtraInfo :: ULONG_PTR
  } deriving (Eq, Show)

instance Storable KBDLLHOOKSTRUCT where
  sizeOf    _ = #{size KBDLLHOOKSTRUCT}
  alignment   = sizeOf

  poke p wc = do
    #{poke KBDLLHOOKSTRUCT, vkCode      } p $ vkCode wc
    #{poke KBDLLHOOKSTRUCT, scanCode    } p $ scanCode wc
    #{poke KBDLLHOOKSTRUCT, flags       } p $ flags wc
    #{poke KBDLLHOOKSTRUCT, time        } p $ kdhs_time wc
    #{poke KBDLLHOOKSTRUCT, dwExtraInfo } p $ dwExtraInfo wc

  peek p = return KBDLLHOOKSTRUCT
    `ap` #{peek KBDLLHOOKSTRUCT, vkCode      } p
    `ap` #{peek KBDLLHOOKSTRUCT, scanCode    } p
    `ap` #{peek KBDLLHOOKSTRUCT, flags       } p
    `ap` #{peek KBDLLHOOKSTRUCT, time        } p
    `ap` #{peek KBDLLHOOKSTRUCT, dwExtraInfo } p

foreign import WINDOWS_CCONV "Windows.h CallNextHookEx"
  c_CallNextHookEx :: HHOOK -> LONG -> WPARAM -> LPARAM -> IO LRESULT

callNextHookEx :: LONG -> WPARAM -> LPARAM -> IO LRESULT
callNextHookEx = c_CallNextHookEx nullHANDLE