{-# LANGUAGE CPP #-}

module Win32.WindowMessage.Window (module Win32.WindowMessage.Window) where

import Control.Monad (ap)
import Foreign
import Foreign.C
import Foreign.Ptr
import Win32.Types
import Win32.GDI

##include "windows_cconv.h"

#include <Windows.h>


----------------------------------------------------------------
-- Window Class
----------------------------------------------------------------

-- The classname must not be deallocated until the corresponding class
-- is deallocated.  For this reason, we represent classnames by pointers
-- and explicitly allocate the className.

type ClassName   = LPCTSTR

-- Note: this is one of those rare functions which doesnt free all
-- its String arguments.
--mkClassName :: String -> ClassName
--mkClassName name = unsafePerformIO (newTString name)

type ClassStyle   = UINT

#{enum ClassStyle,
 , cS_VREDRAW           = CS_VREDRAW
 , cS_HREDRAW           = CS_HREDRAW
 , cS_OWNDC             = CS_OWNDC
 , cS_CLASSDC           = CS_CLASSDC
 , cS_PARENTDC          = CS_PARENTDC
 , cS_SAVEBITS          = CS_SAVEBITS
 , cS_DBLCLKS           = CS_DBLCLKS
 , cS_BYTEALIGNCLIENT   = CS_BYTEALIGNCLIENT
 , cS_BYTEALIGNWINDOW   = CS_BYTEALIGNWINDOW
 , cS_NOCLOSE           = CS_NOCLOSE
 , cS_GLOBALCLASS       = CS_GLOBALCLASS
 }

type WNDPROC = (HWND -> UINT -> WPARAM -> LPARAM -> IO LRESULT)

foreign import WINDOWS_CCONV "wrapper"
  wndProc :: WNDPROC -> IO (FunPtr WNDPROC)

data WNDCLASS = WNDCLASS {
    style :: ClassStyle
  , lpfnWndProc :: FunPtr WNDPROC
  , cbClsExtra :: Word
  , cbWndExtra :: Word
  , hInstance :: HINSTANCE
  , hIcon :: HICON
  , hCursor :: HCURSOR
  , hbrBackground :: HBRUSH
  , lpszMenuName :: LPCTSTR
  , lpszClassName :: LPCTSTR
}

instance Storable WNDCLASS where
  sizeOf    _ = #{size WNDCLASS}
  alignment   = sizeOf

  poke p wc = do
    #{poke WNDCLASS, style} p $ style wc
    #{poke WNDCLASS, lpfnWndProc} p $ lpfnWndProc wc
    #{poke WNDCLASS, cbClsExtra} p $ cbClsExtra wc
    #{poke WNDCLASS, cbWndExtra} p $ cbWndExtra wc
    #{poke WNDCLASS, hInstance} p $ hInstance wc
    #{poke WNDCLASS, hIcon} p $ hIcon wc
    #{poke WNDCLASS, hCursor} p $ hCursor wc
    #{poke WNDCLASS, hbrBackground} p $ hbrBackground wc
    #{poke WNDCLASS, lpszMenuName} p $ lpszMenuName wc
    #{poke WNDCLASS, lpszClassName} p $ lpszClassName wc

  peek p = return WNDCLASS
    `ap` #{peek WNDCLASS, style} p
    `ap` #{peek WNDCLASS, lpfnWndProc} p
    `ap` #{peek WNDCLASS, cbClsExtra} p
    `ap` #{peek WNDCLASS, cbWndExtra} p
    `ap` #{peek WNDCLASS, hInstance} p
    `ap` #{peek WNDCLASS, hIcon} p
    `ap` #{peek WNDCLASS, hCursor} p
    `ap` #{peek WNDCLASS, hbrBackground} p
    `ap` #{peek WNDCLASS, lpszMenuName} p
    `ap` #{peek WNDCLASS, lpszClassName} p

--ToDo!
--To avoid confusion with NULL, WNDCLASS requires you to add 1 to a SystemColor
--(which can be NULL)
-- %fun mkMbHBRUSH :: SystemColor -> MbHBRUSH
-- %code
-- %result ((HBRUSH)($0+1));
foreign import WINDOWS_CCONV unsafe "windows.h RegisterClassW"
  c_RegisterClass :: Ptr WNDCLASS -> IO ATOM

foreign import WINDOWS_CCONV unsafe "windows.h UnregisterClassW"
  c_UnregisterClass :: ClassName -> HINSTANCE -> IO ()


----------------------------------------------------------------
-- Window Style
----------------------------------------------------------------

type WindowStyle   = DWORD

#{enum WindowStyle,
 , wS_OVERLAPPED        = WS_OVERLAPPED
 , wS_POPUP             = WS_POPUP
 , wS_CHILD             = WS_CHILD
 , wS_CLIPSIBLINGS      = WS_CLIPSIBLINGS
 , wS_CLIPCHILDREN      = WS_CLIPCHILDREN
 , wS_VISIBLE           = WS_VISIBLE
 , wS_DISABLED          = WS_DISABLED
 , wS_MINIMIZE          = WS_MINIMIZE
 , wS_MAXIMIZE          = WS_MAXIMIZE
 , wS_CAPTION           = WS_CAPTION
 , wS_BORDER            = WS_BORDER
 , wS_DLGFRAME          = WS_DLGFRAME
 , wS_VSCROLL           = WS_VSCROLL
 , wS_HSCROLL           = WS_HSCROLL
 , wS_SYSMENU           = WS_SYSMENU
 , wS_THICKFRAME        = WS_THICKFRAME
 , wS_MINIMIZEBOX       = WS_MINIMIZEBOX
 , wS_MAXIMIZEBOX       = WS_MAXIMIZEBOX
 , wS_GROUP             = WS_GROUP
 , wS_TABSTOP           = WS_TABSTOP
 , wS_OVERLAPPEDWINDOW  = WS_OVERLAPPEDWINDOW
 , wS_POPUPWINDOW       = WS_POPUPWINDOW
 , wS_CHILDWINDOW       = WS_CHILDWINDOW
 , wS_TILED             = WS_TILED
 , wS_ICONIC            = WS_ICONIC
 , wS_SIZEBOX           = WS_SIZEBOX
 , wS_TILEDWINDOW       = WS_TILEDWINDOW
 }

type WindowStyleEx   = DWORD

#{enum WindowStyleEx,
 , wS_EX_DLGMODALFRAME  = WS_EX_DLGMODALFRAME
 , wS_EX_NOPARENTNOTIFY = WS_EX_NOPARENTNOTIFY
 , wS_EX_TOPMOST        = WS_EX_TOPMOST
 , wS_EX_ACCEPTFILES    = WS_EX_ACCEPTFILES
 , wS_EX_TRANSPARENT    = WS_EX_TRANSPARENT
 , wS_EX_MDICHILD       = WS_EX_MDICHILD
 , wS_EX_TOOLWINDOW     = WS_EX_TOOLWINDOW
 , wS_EX_WINDOWEDGE     = WS_EX_WINDOWEDGE
 , wS_EX_CLIENTEDGE     = WS_EX_CLIENTEDGE
 , wS_EX_CONTEXTHELP    = WS_EX_CONTEXTHELP
 , wS_EX_RIGHT          = WS_EX_RIGHT
 , wS_EX_LEFT           = WS_EX_LEFT
 , wS_EX_RTLREADING     = WS_EX_RTLREADING
 , wS_EX_LTRREADING     = WS_EX_LTRREADING
 , wS_EX_LEFTSCROLLBAR  = WS_EX_LEFTSCROLLBAR
 , wS_EX_RIGHTSCROLLBAR = WS_EX_RIGHTSCROLLBAR
 , wS_EX_CONTROLPARENT  = WS_EX_CONTROLPARENT
 , wS_EX_STATICEDGE     = WS_EX_STATICEDGE
 , wS_EX_APPWINDOW      = WS_EX_APPWINDOW
 , wS_EX_OVERLAPPEDWINDOW = WS_EX_OVERLAPPEDWINDOW
 , wS_EX_PALETTEWINDOW  = WS_EX_PALETTEWINDOW
 }

cW_USEDEFAULT :: Pos
-- See Note [Overflow checking and fromIntegral] in Graphics/Win32/GDI/HDC.hs
cW_USEDEFAULT = fromIntegral (#{const CW_USEDEFAULT} :: Word32)

type Pos = Int

type MbPos = Maybe Pos

foreign import WINDOWS_CCONV "windows.h CreateWindowExW"
  c_CreateWindowEx
    :: WindowStyle -> ClassName -> LPCTSTR -> WindowStyle
    -> Pos -> Pos -> Pos -> Pos
    -> HWND -> HMENU -> HINSTANCE -> LPVOID
    -> IO HWND

foreign import WINDOWS_CCONV "windows.h DefWindowProcW"
  c_DefWindowProc :: HWND -> UINT -> WPARAM -> LPARAM -> IO LRESULT
