{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module Win32.Console (module Win32.Console) where

import Win32.Types
import Foreign.Ptr
import Foreign.C
import Data.Bits

#include <Windows.h>

pattern CTRL_C_EVENT        = #{const CTRL_C_EVENT}
pattern CTRL_BREAK_EVENT    = #{const CTRL_BREAK_EVENT}
pattern CTRL_CLOSE_EVENT    = #{const CTRL_CLOSE_EVENT}
pattern CTRL_LOGOFF_EVENT   = #{const CTRL_LOGOFF_EVENT}
pattern CTRL_SHUTDOWN_EVENT = #{const CTRL_SHUTDOWN_EVENT}

newtype ConsoleMode = ConsoleMode DWORD deriving (Bits, Eq , Show)

pattern ENABLE_PROCESSED_INPUT = ConsoleMode #{const ENABLE_PROCESSED_INPUT}
pattern ENABLE_LINE_INPUT      = ConsoleMode #{const ENABLE_LINE_INPUT}
pattern ENABLE_ECHO_INPUT      = ConsoleMode #{const ENABLE_ECHO_INPUT}
pattern ENABLE_WINDOW_INPUT    = ConsoleMode #{const ENABLE_WINDOW_INPUT}
pattern ENABLE_MOUSE_INPUT     = ConsoleMode #{const ENABLE_MOUSE_INPUT}
pattern ENABLE_INSERT_MODE     = ConsoleMode #{const ENABLE_INSERT_MODE}
pattern ENABLE_QUICK_EDIT_MODE = ConsoleMode #{const ENABLE_QUICK_EDIT_MODE}
pattern ENABLE_EXTENDED_FLAGS  = ConsoleMode #{const ENABLE_EXTENDED_FLAGS}
pattern ENABLE_AUTO_POSITION   = ConsoleMode #{const ENABLE_AUTO_POSITION}

pattern ENABLE_PROCESSED_OUTPUT   = ConsoleMode (#{const ENABLE_PROCESSED_OUTPUT})
pattern ENABLE_WRAP_AT_EOL_OUTPUT = ConsoleMode (#{const ENABLE_WRAP_AT_EOL_OUTPUT})

foreign import WINDOWS_CCONV "Windows.h SetConsoleMode"
  c_SetConsoleMode :: HANDLE -> DWORD -> IO BOOL

newtype StdHandleFlag     = StdHandleFlag DWORD
pattern STD_INPUT_HANDLE  = StdHandleFlag #{const STD_INPUT_HANDLE}
pattern STD_OUTPUT_HANDLE = StdHandleFlag #{const STD_OUTPUT_HANDLE}
pattern STD_ERROR_HANDLE  = StdHandleFlag #{const STD_ERROR_HANDLE}

foreign import WINDOWS_CCONV "Windows.h GetStdHandle"
  c_GetStdHandle :: DWORD -> IO HANDLE

foreign import WINDOWS_CCONV "Windows.h SetConsoleCtrlHandler"
  c_SetConsoleCtrlHandler :: PHANDLER_ROUTINE -> BOOL -> IO BOOL

type HandlerRoutine = DWORD -> IO BOOL
type PHANDLER_ROUTINE = FunPtr HandlerRoutine
foreign import WINDOWS_CCONV "wrapper"
  pHANDLER_ROUTINE :: HandlerRoutine -> IO (FunPtr HandlerRoutine)

#if _WIN32_WINNT >= 0x0501

foreign import WINDOWS_CCONV "Windows.h AddConsoleAliasW"
  c_AddConsoleAlias :: LPCTSTR -> LPCTSTR -> LPCTSTR -> IO BOOL

#endif

foreign import WINDOWS_CCONV "Windows.h AllocConsole"
  c_AllocConsole :: IO BOOL

data COORD = COORD {
    x :: SHORT
  , y :: SHORT
  } deriving (Eq, Ord, Show)

data SMALL_RECT = SMALL_RECT {
    left :: SHORT
  , top :: SHORT
  , right :: SHORT
  , bottom :: SHORT
  } deriving (Eq, Ord, Show)

data KEY_EVENT_RECORD = KEY_EVENT_RECORD {
    bKeyDown :: BOOL
  , wRepeatCount :: WORD
  , wVirtualKeyCode :: WORD
  , wVirtualScanCode :: WORD
  , unionshit :: SHORT -- TODO: union {WCHAR UnicodeChar; CHAR AsciiChar;} uChar;
  , dwControlKeyState :: SHORT
  } deriving (Eq, Ord, Show)

newtype ControlKeyState    = ControlKeyState DWORD deriving (Bits, FiniteBits, Show, Eq)
pattern RIGHT_ALT_PRESSED  = ControlKeyState (#{const RIGHT_ALT_PRESSED})  -- the right alt key is pressed.
pattern LEFT_ALT_PRESSED   = ControlKeyState (#{const LEFT_ALT_PRESSED})   -- the left alt key is pressed.
pattern RIGHT_CTRL_PRESSED = ControlKeyState (#{const RIGHT_CTRL_PRESSED}) -- the right ctrl key is pressed.
pattern LEFT_CTRL_PRESSED  = ControlKeyState (#{const LEFT_CTRL_PRESSED})  -- the left ctrl key is pressed.
pattern SHIFT_PRESSED      = ControlKeyState (#{const SHIFT_PRESSED})      -- the shift key is pressed.
pattern NUMLOCK_ON         = ControlKeyState (#{const NUMLOCK_ON})         -- the numlock light is on.
pattern SCROLLLOCK_ON      = ControlKeyState (#{const SCROLLLOCK_ON})      -- the scrolllock light is on.
pattern CAPSLOCK_ON        = ControlKeyState (#{const CAPSLOCK_ON})        -- the capslock light is on.
pattern ENHANCED_KEY       = ControlKeyState (#{const ENHANCED_KEY})       -- the key is enhanced.
pattern NLS_DBCSCHAR       = ControlKeyState (#{const NLS_DBCSCHAR})       -- DBCS for JPN: SBCS/DBCS mode.
pattern NLS_ALPHANUMERIC   = ControlKeyState (#{const NLS_ALPHANUMERIC})   -- DBCS for JPN: Alphanumeric mode.
pattern NLS_KATAKANA       = ControlKeyState (#{const NLS_KATAKANA})       -- DBCS for JPN: Katakana mode.
pattern NLS_HIRAGANA       = ControlKeyState (#{const NLS_HIRAGANA})       -- DBCS for JPN: Hiragana mode.
pattern NLS_ROMAN          = ControlKeyState (#{const NLS_ROMAN})          -- DBCS for JPN: Roman/Noroman mode.
pattern NLS_IME_CONVERSION = ControlKeyState (#{const NLS_IME_CONVERSION}) -- DBCS for JPN: IME conversion.
pattern NLS_IME_DISABLE    = ControlKeyState (#{const NLS_IME_DISABLE})    -- DBCS for JPN: IME enable/disable.

data MOUSE_EVENT_RECORD = MOUSE_EVENT_RECORD {
    dwMousePosition   :: COORD
  , dwButtonState     :: DWORD
  , dwControlKeyState :: DWORD
  , dwEventFlags      :: DWORD
  } deriving (Eq, Ord, Show)

-- ButtonState flags
newtype ButtonState    = ButtonState DWORD deriving (Bits, FiniteBits, Show, Eq)
pattern FROM_LEFT_1ST_BUTTON_PRESSED = ButtonState (#{const FROM_LEFT_1ST_BUTTON_PRESSED})
pattern RIGHTMOST_BUTTON_PRESSED     = ButtonState (#{const RIGHTMOST_BUTTON_PRESSED})
pattern FROM_LEFT_2ND_BUTTON_PRESSED = ButtonState (#{const FROM_LEFT_2ND_BUTTON_PRESSED})
pattern FROM_LEFT_3RD_BUTTON_PRESSED = ButtonState (#{const FROM_LEFT_3RD_BUTTON_PRESSED})
pattern FROM_LEFT_4TH_BUTTON_PRESSED = ButtonState (#{const FROM_LEFT_4TH_BUTTON_PRESSED})


foreign import WINDOWS_CCONV "Windows.h GetConsoleWindow"
  c_GetConsoleWindow :: IO HWND
