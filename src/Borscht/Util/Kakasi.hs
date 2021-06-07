{-# LANGUAGE ForeignFunctionInterface #-}

module Borscht.Util.Kakasi where

-- foreign
import Foreign
import Foreign.C.Types
import Foreign.C.String
import qualified Foreign.Marshal.Array as FMA

-- text / strings
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text as Text
import qualified Data.ByteString as BS
import qualified Data.Text.Foreign as TF


------------------------------------------------------------

foreign import ccall "math.h sin"
    c_sin :: CDouble -> CDouble

fastSin :: Double -> Double
fastSin x = realToFrac  (c_sin (realToFrac x))

------------------------------------------------------------

-- Related:  StackOverflow, "What is the Encoding of Argv?"
-- https://stackoverflow.com/a/5596232/1444650

withUtf8 :: Text -> (CString -> IO a) -> IO a
withUtf8 = BS.useAsCString . TE.encodeUtf8

-- withStrings :: [Text] -> (Ptr CString -> IO a) -> IO a
-- withStrings ts action = withMany (BS.useAsCString . BS.pack . Text.unpack) ts (`withArray` action)

withUtf8Array :: [Text] -> (Ptr CString -> IO a) -> IO a
withUtf8Array ts action = withMany withUtf8 ts (`withArray` action)

------------------------------------------------------------

-- | Use this `free` function instead of Foreign.Marshall.Alloc.free,
-- as recommended by [this answer](https://stackoverflow.com/a/43386041/1444650).
-- foreign import ccall unsafe "stdlib.h free"
--     c_free :: Ptr a -> IO ()

------------------------------------------------------------

-- | Initialize. Specify the same arguments as when starting kakasi. The
-- example below initializes Kakasi with the same settings as running `kakasi -w'.
--
--     char * argv [] = {"kakasi", "-w"};
--     kakasi_getopt_argv (2, argv);
-- 
-- Returns 0 if the initialization succeeds and 1 if it fails.
-- Remember to kakasi_close_kanwadict() before calling this function for a second time.

foreign import ccall unsafe "libkakasi.h kakasi_getopt_argv"
    -- int kakasi_getopt_argv(int, char **)
    c_kakasi_getopt :: CInt -> Ptr CString -> IO CInt

-- useAsCStringArray :: [BS.ByteString] -> ([CString] -> IO a) -> IO a
-- useAsCStringArray bs action = mapM fn bs where
--     fn = allocaBytes

kakasiConfigure :: [Text] -> IO Bool
kakasiConfigure argv = do
    withUtf8Array argv $ \strs -> do 
        exit <- c_kakasi_getopt (fromIntegral $ length argv) strs
        return (exit == 0)

------------------------------------------------------------

-- | Perform processing. In the argument, pass a pointer to the character string
-- to be processed.  The processing result is returned as a pointer to the string.
-- The returned pointer is to memory malloc'ed inside kakasi_do. 
-- Free this memory when it is no longer needed.

foreign import ccall unsafe "libkakasi.h kakasi_do"
    -- char *kakasi_do(char *)
    c_kakasi_do :: CString -> IO CString

foreign import ccall unsafe "libkakasi.h kakasi_free"
    -- char *kakasi_do(char *)
    c_kakasi_free :: CString -> IO CInt

decodeUtf8 :: CString -> IO Text
decodeUtf8 cstr = do
  bytestr <- BS.packCString cstr
  print bytestr
  return (TE.decodeUtf8 bytestr)

encodeUtf8 :: Text -> (CString -> IO a) -> IO a
encodeUtf8 text = BS.useAsCString (TE.encodeUtf8 text)

kakasiConvert :: Text -> IO Text
kakasiConvert t = withUtf8 t $ \instr -> do
    outstr <- c_kakasi_do instr
    result <- decodeUtf8 outstr
    free outstr
    return result

-- | Closes the dictionary that KAKASI uses by default. kakasi_getopt_argv
-- If you call KAKASI multiple times with different configurations, you must
-- call this function before doing so.
-- Returns 0 if the process succeeds and 1 if the process fails.

foreign import ccall unsafe "libkakasi.h kakasi_close_kanwadict"
    -- int kakasi_close_kanwadict()
    c_kakasi_close_kanwadict :: IO Int

kakasiClose :: IO Bool
kakasiClose = print ("close" :: String) >> (== 0) <$> c_kakasi_close_kanwadict