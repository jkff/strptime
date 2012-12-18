{-# LANGUAGE ForeignFunctionInterface, FlexibleInstances, BangPatterns, CPP #-}
------------------------------------------------------------
-- |
-- Copyright    :   (c) 2009,2010 Eugene Kirpichov
-- License      :   BSD-style
--
-- Maintainer   :   ekirpichov@gmail.com
-- Stability    :   experimental
-- Portability  :   portable (H98 + FFI)
--
-- A binding to strptime with extra features - see below.
--
------------------------------------------------------------

module Data.Time.Parse (
    Strptime(..)
) where

import Foreign
import Foreign.C.Types
import Foreign.C.String
#if __GLASGOW_HASKELL__ >= 704
import qualified Foreign.ForeignPtr.Unsafe as PU
#else
import Foreign.ForeignPtr as PU
#endif
import Foreign.Marshal.Alloc
import GHC.Ptr
import qualified System.IO.Unsafe as U
import Unsafe.Coerce

import Data.Time

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Lazy.Internal as LI
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

#include <time.h>
#include "hstrptime.h"

-- | The class of values from which time may be parsed
class Strptime a where
    -- | Given a format string in the format of strptime (see <http://linux.die.net/man/3/strptime>)
    -- and a data string, parse a date+time value from the data string and also return the remainder
    -- of the data string. We also support a "%OS" format specifier for fractional seconds, because
    -- we are using the strptime from r-project.org. We also support "%^[+-][N]s" for multiples of 
    -- seconds since epoch, for example "%^-3s" is milliseconds since epoch (N can only be 1 digit)
    strptime  :: a -> a -> Maybe (LocalTime, a)

instance Strptime [Char] where
    strptime f = \s -> do
        (t, n) <- ff s
        return (t, drop n s)
      where ff = strptime_ f

instance Strptime L.ByteString where
    strptime f = \s -> do
        (t, n) <- ff s
        return (t, L.drop (fromIntegral n) s)
      where ff = strptime_ f

instance Strptime S.ByteString where
    strptime f = \s -> do
        (t, n) <- ff s
        return (t, S.drop (fromIntegral n) s)
      where ff = strptime_ f

instance  Strptime T.Text where
    strptime f = \s -> do
        (t, n) <- ff s
        return (t, T.drop (fromIntegral n) s)
      where ff = strptime_ f

instance  Strptime TL.Text where
    strptime f = \s -> do
        (t, n) <- ff s
        return (t, TL.drop (fromIntegral n) s)
      where ff = strptime_ f

class Strptime_ a where
    strptime_ :: a -> a -> Maybe (LocalTime, Int)

instance Strptime_ [Char] where
    strptime_ f = let pf = S.pack f in \s -> strptime_ pf (S.pack s)

instance Strptime_ L.ByteString where
    strptime_ f = let pf = S.concat (L.toChunks f) in \s -> strptime_ pf (S.concat . L.toChunks $ s)

instance Strptime_ T.Text where
    strptime_ f = let pf = TE.encodeUtf8 f in \s -> strptime_ pf (TE.encodeUtf8 s)

instance Strptime_ TL.Text where
    strptime_ f = let pf = TLE.encodeUtf8 f in \s -> strptime_ pf (TLE.encodeUtf8 s)

instance Strptime_ S.ByteString where
    strptime_ f = U.unsafePerformIO $ do
      -- Avoid memcpy-ing the format string every time.
      let (pf, ofs, len) = BI.toForeignPtr f
      ztf <- mallocBytes (len+1)
      copyBytes ztf (PU.unsafeForeignPtrToPtr pf) len
      pokeByteOff ztf len (0::Word8)
      fztf <- newForeignPtr_ ztf
      addForeignPtrFinalizer finalizerFree fztf

      return $ \s -> U.unsafePerformIO $ S.useAsCString s $ \cs -> do
        allocaBytes (#const sizeof(struct tm)) $ \p_tm -> do
        alloca $ \p_fsecs -> do
        poke p_fsecs 0
        alloca $ \p_offset -> do
          last <- hstrptime_c cs (castPtr ztf) p_tm p_fsecs p_offset
          if last == nullPtr
            then return Nothing
            else do
              sec   <-  (#peek struct tm,tm_sec  ) p_tm :: IO CInt
              min   <-  (#peek struct tm,tm_min  ) p_tm :: IO CInt
              hour  <-  (#peek struct tm,tm_hour ) p_tm :: IO CInt
              mday  <-  (#peek struct tm,tm_mday ) p_tm :: IO CInt
              month <-  (#peek struct tm,tm_mon  ) p_tm :: IO CInt
              year  <-  (#peek struct tm,tm_year ) p_tm :: IO CInt
              fsecs <-  peek p_fsecs
              let !day = fromGregorian (fromIntegral (year+1900)) (1+fromIntegral month) (fromIntegral mday)
              let !pico = round ((fromIntegral sec + fsecs) * 1000000000000) :: Integer
              let (!h, !m, !s) = (fromIntegral hour, fromIntegral min, unsafeCoerce pico)
              let !tod = TimeOfDay h m s
              
              touchForeignPtr fztf
              
              return $ Just (LocalTime day tod, last `minusPtr` cs)

type CTm = () -- struct tm

foreign import ccall unsafe "hstrptime.h hstrptime"
    hstrptime_c :: CString -> CString -> Ptr CTm -> Ptr Double -> Ptr Int -> IO CString
