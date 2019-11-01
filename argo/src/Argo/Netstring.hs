{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}


-- | A netstring is an ASCII-encoded sequence of digits representing a
-- number N, followed by an ASCII colon, followed by N bytes, followed
-- by an ASCII comma. This allows receivers to know how much is needed.
--
-- Definition: http://cr.yp.to/proto/netstrings.txt
--
-- Netstrings allow malformed JSON to be more robustly detected when
-- using JSON-RPC.
module Argo.Netstring
  ( Netstring
  , encodeNetstring
  , decodeNetstring
  , netstring
  , parseNetstring
  , netstringFromHandle
  )
where

import Control.Exception
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Builder as BS

import Data.Word
import System.IO

import Debug.Trace

data BadNetstring = BadLength | MissingColon (Maybe Word8) | MissingComma (Maybe Word8) deriving Show

instance Exception BadNetstring

newtype Netstring
  = Netstring ByteString
  deriving (Eq, Ord)

instance Show Netstring where
  show (Netstring s) = "(netstring " ++ show s ++ ")"

-- | Construct a new netstring from a bytestring
netstring :: ByteString -> Netstring
netstring = Netstring

-- | Get the underlying bytestring in a netstring
decodeNetstring :: Netstring -> ByteString
decodeNetstring (Netstring s) = s

-- | Encode a netstring as a bytestring, which will contain its length
encodeNetstring :: Netstring -> ByteString
encodeNetstring (Netstring bytes) =
  BS.toLazyByteString $
    BS.stringUtf8 (show (BS.length bytes)) <>
    BS.charUtf8 ':' <>
    BS.lazyByteString bytes <>
    BS.charUtf8 ','

-- >>> toNetstring "hello"
-- "5:hello,"


-- | Read a netstring from a handle. Return Nothing on end of file.
netstringFromHandle :: Handle -> IO (Maybe Netstring)
netstringFromHandle h =
  do eof <- hIsEOF h
     if eof
       then return Nothing
       else Just <$> getNetString
  where
    getNetString =
      do l     <- len Nothing
         bytes <- BS.hGet h l
         c     <- BS.head <$> BS.hGet h 1
         if isComma c
           then return (Netstring bytes)
           else throwIO (MissingComma (Just c))

    len Nothing =
      do x <- BS.head <$> BS.hGet h 1
         if not (isDigit x)
           then throwIO BadLength
           else len (Just (asDigit x))

    len (Just acc) =
      do x <- BS.head <$> BS.hGet h 1
         if | isColon x -> return acc
            | isDigit x -> len (Just (10 * acc + asDigit x))
            | otherwise -> throwIO (MissingColon (Just x))

-- | Attempt to split a ByteString into a prefix that is a valid netstring, and an arbitrary suffix
parseNetstring :: ByteString -> (Netstring, ByteString)
parseNetstring input =
  let (lenBytes, rest) = BS.span isDigit input
      len = asLength lenBytes
  in
    case BS.uncons rest of
      Nothing -> throw (MissingColon Nothing)
      Just (c, rest')
        | isColon c ->
            let (content, rest'') = BS.splitAt (fromIntegral len) rest'
            in
              case BS.uncons rest'' of
                Nothing -> throw (MissingComma Nothing)
                Just (b, done) | isComma b -> (netstring content, done)
                               | otherwise -> throw (MissingComma (Just b))
        | otherwise -> throw (MissingColon (Just c))

isDigit :: Word8 -> Bool
isDigit w = w >= 0x30 && w <= 0x39

isColon :: Word8 -> Bool
isColon w = w == 0x3a

isComma :: Word8 -> Bool
isComma w = w == 0x2c

-- | Assumes isDigit of argument
asDigit :: Word8 -> Int
asDigit w = fromIntegral (w - 0x30)

asLength :: ByteString -> Int
asLength len =
  if BS.null len
    then throw BadLength
    else go 0 (map asDigit (BS.unpack len))
  where
    go acc [] = acc
    go acc (d:ds) = go (acc * 10 + d) ds

-- >>> :set -XOverloadedStrings
-- >>> parseNetstring "5:hello,aldskf"
-- ("hello","aldskf")
