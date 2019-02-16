{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module Win32.Types
  ( module Win32.Types
  , module Foreign.C
  ) where

import Control.Monad (ap)
import Data.Int (Int32, Int64)
import Data.Maybe (fromMaybe)
import Foreign
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)

#if !MIN_VERSION_base(4,8,0)
import Data.Word (Word)
#endif

#include <Windows.h>

----------------------------------------------------------------
-- Platform specific definitions
--
-- Most typedefs and prototypes in Win32 are expressed in terms
-- of these types.  Try to follow suit - it'll make it easier to
-- get things working on Win64 (or whatever they call it on Alphas).
----------------------------------------------------------------

type BOOL          = Bool
type BYTE          = Word8
type UCHAR         = CUChar
type SHORT         = CShort
type USHORT        = CUShort
type UINT          = CUInt
type INT           = Int32
type WORD          = CUShort
type DWORD         = CUInt
type LONG          = Int32
type FLOAT         = Float
type LARGE_INTEGER = Int64

type UINT_PTR      = Word

-- Not really a basic type, but used in many places
type DDWORD        = Word64

----------------------------------------------------------------

type MbString      = Maybe String
type MbINT         = Maybe INT

type ATOM          = UINT
type WPARAM        = CIntPtr
type LPARAM        = CIntPtr
type LRESULT       = LONG
type SIZE_T        = DWORD
type ULONG_PTR     = CIntPtr

type MbATOM        = Maybe ATOM

type HRESULT       = LONG

----------------------------------------------------------------
-- Pointers
----------------------------------------------------------------

type Addr          = Ptr ()

type LPVOID        = CIntPtr
type LPCVOID       = LPVOID
type LPBOOL        = Ptr BOOL
type LPBYTE        = Ptr BYTE
type PUCHAR        = Ptr UCHAR
type LPDWORD       = Ptr DWORD
type LPSTR         = Ptr CChar
type LPCSTR        = LPSTR
type LPWSTR        = Ptr CWchar
type LPCWSTR       = LPWSTR
type LPTSTR        = Ptr TCHAR
type LPCTSTR       = LPTSTR
type LPCTSTR_      = LPCTSTR

-- Optional things with defaults

maybePtr :: Maybe (Ptr a) -> Ptr a
maybePtr = fromMaybe nullPtr

maybeHANDLE :: Maybe HANDLE -> HANDLE
maybeHANDLE = fromMaybe nullHANDLE

ptrToMaybe :: Ptr a -> Maybe (Ptr a)
ptrToMaybe p = if p == nullPtr then Nothing else Just p

maybeNum :: Num a => Maybe a -> a
maybeNum = fromMaybe 0

numToMaybe :: (Eq a, Num a) => a -> Maybe a
numToMaybe n = if n == 0 then Nothing else Just n

type MbLPVOID      = Maybe LPVOID
type MbLPCSTR      = Maybe LPCSTR
type MbLPCTSTR     = Maybe LPCTSTR

----------------------------------------------------------------
-- Chars and strings
----------------------------------------------------------------

withTString    :: String -> (LPTSTR -> IO a) -> IO a
withTStringLen :: String -> ((LPTSTR, Int) -> IO a) -> IO a
peekTString    :: LPCTSTR -> IO String
peekTStringLen :: (LPCTSTR, Int) -> IO String
newTString     :: String -> IO LPCTSTR

-- UTF-16 version:
type TCHAR     = CWchar
withTString    = withCWString
withTStringLen = withCWStringLen
peekTString    = peekCWString
peekTStringLen = peekCWStringLen
newTString     = newCWString

{- ANSI version:
type TCHAR     = CChar
withTString    = withCString
withTStringLen = withCStringLen
peekTString    = peekCString
peekTStringLen = peekCStringLen
newTString     = newCString
-}

----------------------------------------------------------------
-- Handles
----------------------------------------------------------------

newtype HANDLE = HANDLE CIntPtr deriving (Eq, Show, Storable)
type ForeignHANDLE = ForeignPtr ()

type HWND = HANDLE

type   HKEY        = ForeignHANDLE
type   PKEY        = HANDLE

nullHANDLE :: HANDLE
nullHANDLE = HANDLE (CIntPtr 0)

type MbHANDLE      = Maybe HANDLE

type   HINSTANCE   = Ptr ()
type MbHINSTANCE   = Maybe HINSTANCE

type   HMODULE     = Ptr ()
type MbHMODULE     = Maybe HMODULE

nullFinalHANDLE :: ForeignPtr a
nullFinalHANDLE = unsafePerformIO (newForeignPtr_ nullPtr)

pattern INVALID_HANDLE_VALUE :: HANDLE
pattern INVALID_HANDLE_VALUE = HANDLE (CIntPtr (-1))

type ErrCode = DWORD


type LCID = DWORD

type LANGID = WORD
type SortID = WORD

-- Windef.h

data POINT = POINT {
    x :: LONG
  , y :: LONG
  } deriving (Eq, Ord, Show)

instance Storable POINT where
  sizeOf    _ = #{size POINT}
  alignment   = sizeOf

  poke p pt = do
    #{poke POINT, x} p $ x pt
    #{poke POINT, y} p $ y pt

  peek p = return POINT
    `ap` #{peek POINT, x} p
    `ap` #{peek POINT, y} p
