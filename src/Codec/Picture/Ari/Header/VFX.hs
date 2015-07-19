{-# LANGUAGE BangPatterns #-}
module Codec.Picture.Ari.Header.VFX where

-- VFX stands for Camera Device Information

import Data.Binary
import Data.Binary.Get
import Data.Binary.IEEE754
import Control.Applicative
import Control.Monad
import qualified Data.ByteString as BS

-- |Ari header data structure
data AriHeaderVFXv8 =
  AriHeaderVFXv8
    { ariVFXValid           :: !Int
    , ariGpsLatitude        :: !BS.ByteString -- 8
    , ariGpsLongitude       :: !BS.ByteString -- 8
    , ariX                  :: !Float
    , ariY                  :: !Float
    , ariZ                  :: !Float
    , ariPan                :: !Float
    , ariTilt               :: !Float
    , ariRoll               :: !Float
    , ariMasterFlag         :: !Int
    , ariChannelInfo        :: !Int
    , ariPartnerID          :: !Int
    }

-- |Print functions
instance Show AriHeaderVFXv8 where
  show a = "VFX valid: " ++ show (ariVFXValid a)
        ++ "\ngps latitude: " ++ show (ariGpsLatitude a)
        ++ "\ngps longitude: " ++ show (ariGpsLongitude a)
        ++ "\nposition x: " ++ show (ariX a)
        ++ "\nposition y: " ++ show (ariY a)
        ++ "\nposition z: " ++ show (ariZ a)
        ++ "\npan: " ++ show (ariPan a)
        ++ "\ntilt: " ++ show (ariTilt a)
        ++ "\nroll: " ++ show (ariRoll a) 

-- |Binary layout
decodeAriHeaderVFXv8 :: Get AriHeaderVFXv8
decodeAriHeaderVFXv8 = 
  AriHeaderVFXv8
    <$> liftM fromIntegral getWord32le                  -- ariVFXValid          0x0438-0x043B
    <*> getByteString 8                                 -- ariGpsLatitude       0x043C-0x0443            
    <*> getByteString 8                                 -- ariGpsLongitude      0x0444-0x044B 
    <*> getFloat32le                                    -- ariX                 0x044C-0x044F
    <*> getFloat32le                                    -- ariY                 0x0450-0x0453 
    <*> getFloat32le                                    -- ariZ                 0x0454-0x0457 
    <*> getFloat32le                                    -- ariPan               0x0458-0x045B 
    <*> getFloat32le                                    -- ariTilt              0x045C-0x045F 
    <*> getFloat32le                                    -- ariRoll              0x0460-0x0463
    <*> liftM fromIntegral getWord32le                  -- ariMasterFlag        0x0464-0x0467
    <*> liftM fromIntegral getWord32le                  -- ariChannelInfo       0x0468-0x046B 
    <*> (liftM fromIntegral getWord32le                 -- ariPartnerID         0x046C-0x046F
    <* skip 128)                                        -- Reserved             0x0470-0x04EF


