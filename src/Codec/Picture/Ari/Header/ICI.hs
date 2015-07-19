{-# LANGUAGE BangPatterns #-}
module Codec.Picture.Ari.Header.ICI where

-- ICI stands for Image Content Information

import Data.Binary
import Data.Binary.Get
import Data.Binary.IEEE754
import Control.Applicative
import Control.Monad
import qualified Data.ByteString as BS
import Codec.Picture.Ari.Types

--
-- TODO : color matrix encoding to find out

-- |Ari header data structure
data AriHeaderICIv8 =
  AriHeaderICIv8
    { ariICIValid           :: !Int
    , ariICIVersion         :: !BS.ByteString
    , ariWhiteBalance       :: !Int
    , ariGreenTintFactor    :: !Int
    , ariWhiteBalanceFactorR:: !Float
    , ariWhiteBalanceFactorG:: !Float
    , ariWhiteBalanceFactorB:: !Float
    , ariWBAppliedInCamera  :: !Int
    , ariExposureIndex      :: !Int
    , ariBlackLevel         :: !Float
    , ariWhiteLevel         :: !Float
    -- Color matrix as a raw bytestring
    , ariColorMatrix        :: !BS.ByteString
    -- , ariColorMatrix00      :: !Float
    -- , ariColorMatrix01      :: !Float
    -- , ariColorMatrix02      :: !Float
    -- , ariColorMatrix10      :: !Float
    -- , ariColorMatrix11      :: !Float
    -- , ariColorMatrix12      :: !Float
    -- , ariColorMatrix20      :: !Float
    -- , ariColorMatrix21      :: !Float
    -- , ariColorMatrix22      :: !Float
    , ariColorMatrixDesatG  :: !Float
    , ariColorMatrixDesatO  :: !Float
    , ariHLDesatFlag        :: !Int
    , ariTargetColorSpace   :: !Int
    , ariSharpness          :: !Int
    , ariPixelAspectRatio   :: !Float
    , ariFlip               :: !Int
    , ariLookFile           :: !AriString
    , ariLookLutMode        :: !Int
    , ariLookLutOffset      :: !Int
    , ariLookLutSize        :: !Int
    , ariLookLutCRC         :: !Int
    , ariLinearSaturation   :: !Int
    , ariCdlSlopeR          :: !Float
    , ariCdlSlopeG          :: !Float
    , ariCdlSlopeB          :: !Float
    , ariCdlOffsetR         :: !Float 
    , ariCdlOffsetG         :: !Float 
    , ariCdlOffsetB         :: !Float 
    , ariCdlPowerR          :: !Float
    , ariCdlPowerG          :: !Float
    , ariCdlPowerB          :: !Float
    , ariPrinterLights      :: !BS.ByteString -- 6 bytes...
    , ariCdlApplicationMode :: !Int
    } 

-- |Print functions
instance Show AriHeaderICIv8 where
  show a = "ICI valid: "                 ++ show (ariICIValid a)
        ++ "\nwhite balance: "           ++ show (ariWhiteBalance a) ++ "K"
        ++ "\nwhite balance factors: "   ++ show (ariWhiteBalanceFactorR a)
        ++ " "   ++ show (ariWhiteBalanceFactorG a)
        ++ " "   ++ show (ariWhiteBalanceFactorB a)
        ++ "\nwhite balance applied: "   ++ show (ariWBAppliedInCamera a)
        ++ "\nexposure index: "          ++ show (ariExposureIndex a)
        ++ "\nblack level: "             ++ show (ariBlackLevel a)
        ++ "\nwhite level: "             ++ show (ariWhiteLevel a)
        ++ "\nsharpness: "               ++ show (ariSharpness a)
        -- ++ "\ncolor matrix: "            ++ show (ariColorMatrix00 a)
        -- ++ " " ++ show (ariColorMatrix01 a) ++ " " ++ show (ariColorMatrix02 a)
        ++ "\npixel aspect ratio: "      ++ show (ariPixelAspectRatio a)
        ++ "\ntarget color space: "      ++ show (ariTargetColorSpace a)
        ++ "\nlook file: "               ++ show (ariLookFile a)
        ++ "\ndesat gain: "              ++ show (ariColorMatrixDesatG a)
        ++ "\ndesat offset: "            ++ show (ariColorMatrixDesatO a)
        ++ "\nflip : "                   ++ show (ariFlip a)
        ++ "\nlook mode : "              ++ show (ariLookLutMode a)
        ++ "\nlook offset : "            ++ show (ariLookLutOffset a)


-- |Binary layout
decodeAriHeaderICIv8 :: Get AriHeaderICIv8
decodeAriHeaderICIv8 = 
  AriHeaderICIv8
    <$> liftM fromIntegral getWord32le                  -- ariICIValid              Valid           0x0054-0x0057
    <*> getByteString 4                                 -- ariVersion               Version         0x0058-0x005B
    <*> liftM fromIntegral getWord32le                  -- ariWhiteBalance          WhiteBalance    0x005C-0x005F 
    <*> liftM fromIntegral getWord32le                  -- ariGreenTintFactor       GreenTintFactor 0x0060-0x0063 
    <*> getFloat32le                                    -- ariWhiteBalanceFactorR   WB R            0x0064-0x0067
    <*> getFloat32le                                    -- ariWhiteBalanceFactorG   WB G            0x0068-0x006B
    <*> getFloat32le                                    -- ariWhiteBalanceFactorB   WB B            0x006C-0x006F
    <*> liftM fromIntegral getWord32le                  -- ariWBAppliedInCamera     WB Applied      0x0070-0x0073
    <*> liftM fromIntegral getWord32le                  -- ariExposureIndex         Exposure Index  0x0074-0x0077
    <*> getFloat32le                                    -- ariBlackLevel            Black Level     0x0078-0x007B
    <*> getFloat32le                                    -- ariWhiteLevel            White Level     0x007C-0x007F
                                                        --                          Color matrix    0x0080-0x00AF         
    <*> getByteString 48
    -- <*> getFloat32le                                    -- ariColorMatrix00
    -- <*> getFloat32le                                    -- ariColorMatrix01
    -- <*> getFloat32le                                    -- ariColorMatrix02
    -- <*> getFloat32le                                    -- ariColorMatrix10
    -- <*> getFloat32le                                    -- ariColorMatrix11
    -- <*> getFloat32le                                    -- ariColorMatrix12
    -- <*> getFloat32le                                    -- ariColorMatrix20
    -- <*> getFloat32le                                    -- ariColorMatrix21
    -- <*> getFloat32le                                    -- ariColorMatrix22
    <*> getFloat32le                                    -- ariColorMatrixDesatG     Desat Gain      0x00B0-0x00B3 
    <*> getFloat32le                                    -- ariColorMatrixDesatO     Desat Offset    0x00B4-0x00B7
    <*> liftM fromIntegral getWord32le                  -- ariHLDesatFlag           Highlight desat 0x00B8-0x00BB
    <*> liftM fromIntegral getWord32le                  -- ariTargetColorSpace      Target CS       0x00BC-0x00BF
    <*> liftM fromIntegral getWord32le                  -- ariSharpness             Sharpness       0x00C0-0x00C3
    <*> getFloat32le                                    -- ariPixelAspectRatio      Pix Aspect      0x00C4-0x00C7
    <*> liftM fromIntegral getWord32le                  -- ariFlip                  Flip            0X00C8-0x00CB
    <*> getAriString 32                                -- ariLookFile              Look file       0x00CC-0x00EB
    <*> liftM fromIntegral getWord32le                  -- ariLookLutMode           Lut mode        0x00EC-0x00EF
    <*> liftM fromIntegral getWord32le                  -- ariLookLutOffset                         0x00F0-0x00F3
    <*> liftM fromIntegral getWord32le                  -- ariLookLutSize                           0x00F4-0x00F7
    <*> liftM fromIntegral getWord32le                  -- ariLookLutCRC                            0x00F8-0x00FB
    <*> liftM fromIntegral getWord32le                  -- ariLinearSaturation                      0x00FC-0x00FF
                                                        -- ariCdlSlope 3 floats                     0x0100-0x010B
    <*> getFloat32le                                    -- ariCdlSlopeR
    <*> getFloat32le                                    -- ariCdlSlopeG
    <*> getFloat32le                                    -- ariCdlSlopeB
                                                        -- ariCdlOffset 3 floats                    0x010C-0x0117
    <*> getFloat32le                                    -- ariCdlOffsetR
    <*> getFloat32le                                    -- ariCdlOffsetG
    <*> getFloat32le                                    -- ariCdlOffsetB
                                                        -- ariCdlPower 3 floats                     0x0118-0x0123
    <*> getFloat32le                                    -- ariCdlPowerR
    <*> getFloat32le                                    -- ariCdlPowerG
    <*> getFloat32le                                    -- ariCdlPowerB
    <*> getByteString 6                                 -- ariPrinterLights                         0x0124-0x0129
    <*> (liftM fromIntegral getWord32le                 -- ariCdlApplicationMode                    0x0130-0x0133
    <* skip 50)                                         -- Reserved                                 0x0134-0x015F

