module Codec.Picture.Ari.Types where

import Control.Applicative
import Data.Binary
import Data.Binary.Get
import Data.ByteString.UTF8 (toString)
import Data.List (intersperse)
import Numeric
import qualified Data.ByteString as BS
--
-- |AriString 
-- is basically a sequence of bytes that we further interpret as a utf-8 string
newtype AriString = AriString BS.ByteString 

instance Show AriString where
  --show (AriString s) = unpack ( decodeUtf8 s )
  -- need to remove \0 characters
  show (AriString s) = takeWhile (\x-> x /= '\NUL') $ toString s 

-- |Helper function to read a ByteString and wrap it in a AriString
getAriString :: Int -> Get AriString
getAriString n = fmap (\x -> AriString x) $ getByteString n
{-# INLINE getAriString #-}


--
-- |AriTimeCode 128
-- Should follow the SMPTE 12M-2 norm
-- Longitudinal timecode data format packed in 128 bits
newtype AriTimeCode128 = AriTimeCode128 BS.ByteString

instance Show AriTimeCode128 where
  -- show in hex atm
  show (AriTimeCode128 b) = 
     -- it seems that each component of the tc is encoded
     -- in a word, starting by the frame
     -- This is a hack !!!
     let binDecNum =  map (flip showHex "") (BS.unpack b)
         arriTC = reverse $ take 4 $ binDecNum 
     in concat $ intersperse ":" $ arriTC

getAriTimeCode128 :: Get AriTimeCode128
getAriTimeCode128 = fmap (\x -> AriTimeCode128 x) $ getByteString 16
{-# INLINE getAriTimeCode128 #-}

-- | 
data AriTimeCode64 = AriTimeCode64 Word8 Word8 Word8 Word8 Word8 Word8 Word8 Word8 
instance Show AriTimeCode64 where
  show (AriTimeCode64 a b c _ _ _ _ _) = showHex a "" ++ showHex b "" ++ showHex c ""

getAriTimeCode64 :: Get AriTimeCode64
getAriTimeCode64 = 
  AriTimeCode64 
      <$> getWord8
      <*> getWord8
      <*> getWord8
      <*> getWord8
      <*> getWord8
      <*> getWord8
      <*> getWord8
      <*> getWord8


-- still TODO :
-- CameraType to text
-- CDI cameraType
--
--CameraId to text
--CameraIndex to text
--
--CameraSystemImage to date
--
--CameraSystemImage Creation to time code
--
--exposure time int  to ms
--
--shutter angle /1000
--sensor fps/1000
--
--Project fps /1000
--
--Master TC make sure all number are 2 padded
--
-- Storage media serial to text
-- or number
--
-- SMTPE UMID as hexa
--
-- TargetColor from int to text
--
-- Lens Focus unit int to enum "Meter" "blabla"
--
-- Lens iris incorrect
--
--
-- Test camera tilt camera roll
