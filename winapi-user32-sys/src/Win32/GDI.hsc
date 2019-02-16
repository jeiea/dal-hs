
module Win32.GDI
  ( module Win32.GDI
  , HWND
  ) where

import Win32.Types

type HMENU   = HANDLE
type HBITMAP = HANDLE
type HFONT   = HANDLE
type HICON   = HANDLE
type HCURSOR = HICON


-- This is not the only handle / resource that should be
-- finalised for you, but it's a start.
-- ToDo.

type HRGN = ForeignHANDLE
type PRGN = HANDLE

type HPALETTE = HANDLE
type HBRUSH   = HANDLE
type HPEN     = HANDLE
type HACCEL   = HANDLE
type HDC      = HANDLE
type HDWP     = HANDLE
