{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types where

import RIO
import Win32.User32 hiding (wParam)

data App = App
  { logFunc :: !LogFunc
  , context :: !(SomeRef KeyProcCtx)
  }

instance HasLogFunc App where
  logFuncL = lens logFunc (\app v -> app { logFunc = v })

instance HasStateRef KeyProcCtx App where
  stateRefL = lens context (\app v -> app { context = v })

data KeyProcCtx = KeyProcCtx
  { spacePressTime :: Maybe DWORD
  , kdhsAfterSpace :: [KBDLLHOOKSTRUCT]
  , delegate :: KeyboardProc
  }

type KeyboardProc = WPARAM -> KBDLLHOOKSTRUCT -> RIO App Bool
