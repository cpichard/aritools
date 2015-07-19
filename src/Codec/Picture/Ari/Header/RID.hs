{-# LANGUAGE BangPatterns #-}
module Codec.Picture.Ari.Header.RID where

import Data.Binary
import Data.Binary.Get
import Control.Applicative
import Control.Monad
import qualified Data.ByteString as BS

-- |Ari header data structure
--  First bytes of the file
data AriHeaderRID =
  AriHeaderRID
    { ariMagic              :: !BS.ByteString   
    , ariByteOrder          :: !Word32 --  0x12345678
    , ariHeaderSize         :: !Int
    , ariVersionNumber      :: !Int
    }

-- |Endianness 
isLittleEndian :: AriHeaderRID -> Bool
isLittleEndian a = (ariByteOrder a) == 0x12345678

readEndianness :: Get Word32
readEndianness = getWord32be

showEndianness :: AriHeaderRID -> String
showEndianness a = if isLittleEndian a
                    then "little endian"
                    else "big endian"
-- |Prints RID
instance Show AriHeaderRID where
  show a = "magic: "                     ++ show (ariMagic a)
        ++ "\nbyte order code: "         ++ show (ariByteOrder a)
        ++ "\nbyte order: "              ++ showEndianness a 
        ++ "\nheader size: "             ++ show (ariHeaderSize a)
        ++ "\nversion number: "          ++ show (ariVersionNumber a)

-- |Binary layout
decodeAriHeaderRID :: Get AriHeaderRID
decodeAriHeaderRID = 
    --TODO use do instead of applicative
  AriHeaderRID
    <$> getByteString 4                                 -- ariMagic             MagicNum        0x0000-0x0003
    <*> readEndianness                                  -- ariByteOrder         ByteOrder       0x0004-0x0007
    <*> liftM fromIntegral getWord32le                  -- ariHeaderSize        HeaderSize      0x0008-0x000B
    <*> liftM fromIntegral getWord32le                  -- ariVersionNumber     VersionNumber   0x000C-0x000F

