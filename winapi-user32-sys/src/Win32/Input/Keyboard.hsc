{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}

module Win32.Input.Keyboard (
    module Win32.Input.Keyboard
  , module Win32.Input.Keyboard.Constants
  ) where

import Control.Monad (ap)
import Data.Bits
import Data.Traversable
import Foreign
import Foreign.C
import Foreign.Marshal.Array
import Win32.Types
import Win32.Input.Keyboard.Constants

#include <Windows.h>

#if __GLASGOW_HASKELL__ < 800
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif

type LPINPUT = Ptr INPUT

foreign import WINDOWS_CCONV "Windows.h SendInput"
  c_SendInput :: UINT -> LPINPUT -> CInt -> IO UINT

c_SendInput' :: UINT -> LPINPUT -> IO UINT
c_SendInput' len p = c_SendInput len p #{size INPUT}

pattern INPUT_MOUSE    :: DWORD
pattern INPUT_MOUSE    = #{const INPUT_MOUSE}
pattern INPUT_KEYBOARD :: DWORD
pattern INPUT_KEYBOARD = #{const INPUT_KEYBOARD}
pattern INPUT_HARDWARE :: DWORD
pattern INPUT_HARDWARE = #{const INPUT_HARDWARE}

pattern KEYEVENTF_EXTENDEDKEY :: DWORD
pattern KEYEVENTF_EXTENDEDKEY = #{const KEYEVENTF_EXTENDEDKEY}
pattern KEYEVENTF_KEYUP       :: DWORD
pattern KEYEVENTF_KEYUP       = #{const KEYEVENTF_KEYUP}
pattern KEYEVENTF_SCANCODE    :: DWORD
pattern KEYEVENTF_SCANCODE    = #{const KEYEVENTF_SCANCODE}
pattern KEYEVENTF_UNICODE     :: DWORD
pattern KEYEVENTF_UNICODE     = #{const KEYEVENTF_UNICODE}

-- TODO: OverloadedRecordFields
data INPUT = MINPUT MOUSEINPUT | KINPUT KEYBDINPUT | HINPUT HARDWAREINPUT
  deriving Show
data MOUSEINPUT = MOUSEINPUT
  { dx             :: LONG
  , dy             :: LONG
  , mouseData      :: DWORD
  , mi_dwFlags     :: DWORD
  , mi_time        :: DWORD
  , mi_dwExtraInfo :: ULONG_PTR
  } deriving Show
data KEYBDINPUT = KEYBDINPUT
  { wVk            :: WORD
  , wScan          :: WORD
  , ki_dwFlags     :: DWORD
  , ki_time        :: DWORD
  , ki_dwExtraInfo :: ULONG_PTR
  } deriving Show
data HARDWAREINPUT = HARDWAREINPUT
  { uMsg    :: DWORD
  , wParamL :: WORD
  , wParamH :: WORD
  } deriving Show

instance Storable INPUT where
  sizeOf    _ = #{size INPUT}
  alignment _ = #{alignment INPUT}

  peek p = do
    tag <- peekByteOff p 0
    case tag of
      INPUT_MOUSE    -> #{peek INPUT, mi} p
      INPUT_KEYBOARD -> #{peek INPUT, ki} p
      INPUT_HARDWARE -> #{peek INPUT, hi} p

  poke p (MINPUT mi) = do
    pokeByteOff p 0 INPUT_MOUSE
    #{poke INPUT, mi} p mi

  poke p (KINPUT ki) = do
    pokeByteOff p 0 INPUT_KEYBOARD
    #{poke INPUT, ki} p ki

  poke p (HINPUT hi) = do
    pokeByteOff p 0 INPUT_HARDWARE
    #{poke INPUT, hi} p hi


instance Storable MOUSEINPUT where
  sizeOf    _ = #{size MOUSEINPUT}
  alignment _ = #{alignment MOUSEINPUT}

  peek p = return MOUSEINPUT
    `ap` #{peek MOUSEINPUT, dx} p
    `ap` #{peek MOUSEINPUT, dy} p
    `ap` #{peek MOUSEINPUT, mouseData} p
    `ap` #{peek MOUSEINPUT, dwFlags} p
    `ap` #{peek MOUSEINPUT, time} p
    `ap` #{peek MOUSEINPUT, dwExtraInfo} p

  poke p (MOUSEINPUT dx dy mouseData dwFlags time dwExtraInfo) = do
    #{poke MOUSEINPUT, dx} p dx
    #{poke MOUSEINPUT, dy} p dy
    #{poke MOUSEINPUT, mouseData} p mouseData
    #{poke MOUSEINPUT, dwFlags} p dwFlags
    #{poke MOUSEINPUT, time} p time
    #{poke MOUSEINPUT, dwExtraInfo} p dwExtraInfo

instance Storable KEYBDINPUT where
  sizeOf    _ = #{size KEYBDINPUT}
  alignment _ = #{alignment KEYBDINPUT}

  peek p = return KEYBDINPUT
    `ap` #{peek KEYBDINPUT, wVk} p
    `ap` #{peek KEYBDINPUT, wScan} p
    `ap` #{peek KEYBDINPUT, dwFlags} p
    `ap` #{peek KEYBDINPUT, time} p
    `ap` #{peek KEYBDINPUT, dwExtraInfo} p

  poke p (KEYBDINPUT wVk wScan dwFlags time dwExtraInfo) = do
    #{poke KEYBDINPUT, wVk} p wVk
    #{poke KEYBDINPUT, wScan} p wScan
    #{poke KEYBDINPUT, dwFlags} p dwFlags
    #{poke KEYBDINPUT, time} p time
    #{poke KEYBDINPUT, dwExtraInfo} p dwExtraInfo

instance Storable HARDWAREINPUT where
  sizeOf    _ = #{size HARDWAREINPUT}
  alignment _ = #{alignment HARDWAREINPUT}

  peek p = return HARDWAREINPUT
    `ap` #{peek HARDWAREINPUT, uMsg} p
    `ap` #{peek HARDWAREINPUT, wParamL} p
    `ap` #{peek HARDWAREINPUT, wParamH} p

  poke p (HARDWAREINPUT uMsg wParamL wParamH) = do
    pokeByteOff p 0 INPUT_HARDWARE
    #{poke HARDWAREINPUT, uMsg} p uMsg
    #{poke HARDWAREINPUT, wParamL} p wParamL
    #{poke HARDWAREINPUT, wParamH} p wParamH
    where p = plusPtr p (sizeOf (0 :: DWORD))

foreign import WINDOWS_CCONV "Windows.h GetKeyState"
  c_GetKeyState :: CInt -> IO SHORT


foreign import WINDOWS_CCONV "Windows.h MapVirtualKeyW"
  c_MapVirtualKeyW
    :: UINT -- uCode
    -> UINT -- uMapType
    -> IO UINT

-- pattern MAPVK_VK_TO_CHAR :: WORD
pattern MAPVK_VK_TO_CHAR = #{const MAPVK_VK_TO_CHAR}
-- pattern MAPVK_VK_TO_VSC :: WORD
pattern MAPVK_VK_TO_VSC = #{const MAPVK_VK_TO_VSC}
-- pattern MAPVK_VSC_TO_VK :: WORD
pattern MAPVK_VSC_TO_VK = #{const MAPVK_VSC_TO_VK}
-- pattern MAPVK_VSC_TO_VK_EX :: WORD
pattern MAPVK_VSC_TO_VK_EX = #{const MAPVK_VSC_TO_VK_EX}

foreign import WINDOWS_CCONV "Windows.h GetKeyNameTextW"
  c_GetKeyNameTextW :: LONG -> LPTSTR -> CInt -> IO CInt

pattern KF_ALTDOWN = #{const KF_ALTDOWN}
pattern KF_DLGMODE = #{const KF_DLGMODE}
pattern KF_EXTENDED = #{const KF_EXTENDED}
pattern KF_MENUMODE = #{const KF_MENUMODE}
pattern KF_REPEAT = #{const KF_REPEAT}
pattern KF_UP = #{const KF_UP}

pattern LLKHF_EXTENDED = 0x1
pattern LLKHF_LOWER_IL_INJECTED = 0x2
pattern LLKHF_INJECTED = #{const LLKHF_INJECTED}
pattern LLKHF_ALTDOWN = 0x10
pattern LLKHF_UP = 0x80
