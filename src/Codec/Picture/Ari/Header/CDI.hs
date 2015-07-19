{-# LANGUAGE BangPatterns #-}
module Codec.Picture.Ari.Header.CDI where

-- CDI stands for Camera Device Information

import Data.Binary
import Data.Binary.Get
import Control.Applicative
import Control.Monad
import qualified Data.ByteString as BS
import Data.List (intersperse)
import Codec.Picture.Ari.Types

-- |Ari header data structure
data AriHeaderCDIv8 =
  AriHeaderCDIv8
    { ariCDIValid           :: !Int
    , ariCameraTypeId       :: !Int
    , ariCameraRevision     :: !Int
    , ariFirmwareVersion    :: !Int
    , ariCameraSN           :: !Int
    , ariCameraID           :: !Int
    , ariCameraIndex        :: !Int
    , ariSysImgCreationTime :: !Word64 -- 8 bytes
    , ariUserDate           :: !Word64 -- 8 bytes
    , ariExposureTime       :: !Int
    , ariShutterAngle       :: !Int
    , ariSpeedRampDuration  :: !Int -- not in use
    , ariSpeedRampStart     :: !Int -- not in use
    , ariSpeedRampEnd       :: !Int -- not in use
    , ariSensorFPS          :: !Int --
    , ariProjectFPS         :: !Int
    , ariMasterTC           :: !AriTimeCode128
    , ariExtLTCFreeRun      :: !AriTimeCode128 -- 16 bytes
    , ariVITCFreeRun        :: !AriTimeCode128 -- 16 bytes -- Not active
    , ariIntFreeRunToDTC    :: !AriTimeCode128
    , ariIntFreeRunUDDTC    :: !AriTimeCode128
    , ariRecRunEdgeCodeTC   :: !AriTimeCode128
    , ariRecRunClipCodeTC   :: !AriTimeCode128
    , ariRecRunRegenTC      :: !AriTimeCode128
    , ariMagazineSN         :: !BS.ByteString
    , ariSMPTEUMID          :: !BS.ByteString
    , ariCameraType         :: !AriString
    , ariRecorderType       :: !AriString
    , ariMirrorShutterRun   :: !BS.ByteString -- 1 byte
    , ariVariFrame          :: !BS.ByteString -- 1 byte
    , ariExposurePhase      :: !BS.ByteString -- 1 byte
    , ariSubSecondFrameCnt  :: !BS.ByteString -- 1 byte
    , ariUUID               :: !BS.ByteString -- 16 bytes
    , ariSUPVersion         :: !AriString
    } 

-- |Print functions
instance Show AriHeaderCDIv8 where
  show a = let cameraInfo = [ "valid: " ++ show (ariCDIValid a)
                            , "camera type id: " ++ show (ariCameraTypeId a)
                            , "camera revision: " ++ show (ariCameraRevision a)
                            , "camera firmware version: " ++ show (ariFirmwareVersion a)
                            , "camera serial number: " ++ show (ariCameraSN a)
                            , "camera index: " ++ show (ariCameraIndex a)
                            , "firmware version: " ++ show (ariFirmwareVersion a)
                            , "sys image creation time: " ++ show (ariSysImgCreationTime a)
                            , "user date: " ++ show (ariUserDate a)
                            , "exposure time: " ++ show (ariExposureTime a)
                            , "shutter angle: " ++ show (ariShutterAngle a)
                            , "project fps: " ++ show (ariProjectFPS a)
                            , "recorder type: " ++ show (ariRecorderType a)
                            -- give error wrong utf8, "magazine serial number: " ++ unpack (decodeUtf8 (ariMagazineSN a))
                            , "smpte uid: " ++ show (ariSMPTEUMID a)
                            , "sup version: " ++ show (ariSUPVersion a)
                            , "master tc: " ++  show (ariMasterTC a)

                            ]
                            
           in concat $ intersperse "\n" cameraInfo 

-- |Binary layout
decodeAriHeaderCDIv8 :: Get AriHeaderCDIv8
decodeAriHeaderCDIv8 = 
  AriHeaderCDIv8
    <$> liftM fromIntegral getWord32le                  -- ariCDIValid                          0x0160-0x0163   
    <*> liftM fromIntegral getWord32le                  -- ariCameraTypeId                      0x0164-0x0167
    <*> liftM fromIntegral getWord32le                  -- ariCameraRevision                    0x0168-0x016B
    <*> liftM fromIntegral getWord32le                  -- ariFirmwareVersion                   0x016C-0x016F
    <*> liftM fromIntegral getWord32le                  -- ariCameraSN                          0x0170-0x0173 
    <*> liftM fromIntegral getWord32le                  -- ariCameraID                          0x0174-0x0177
    <*> liftM fromIntegral getWord32le                  -- ariCameraIndex                       0x0178-0x017B
    <*> getWord64le                                     -- ariSysImgCreationTime                0x017C-0x0183
    <*> getWord64le                                     -- ariUserDate                          0x0184-0x018B
    <*> liftM fromIntegral getWord32le                  -- ariExposureTime                      0x018C-0x018F
    <*> liftM fromIntegral getWord32le                  -- ariShutterAngle                      0x0190-0x0193
    <*> liftM fromIntegral getWord32le                  -- ariSpeedRampDuration                 0x0194-0x0197
    <*> liftM fromIntegral getWord32le                  -- ariSpeedRampStart                    0x0198-0x019B
    <*> liftM fromIntegral getWord32le                  -- ariSpeedRampEnd                      0x019C-0x019F
    <*> liftM fromIntegral getWord32le                  -- ariSensorFPS                         0x01A0-0x01A3
    <*> liftM fromIntegral getWord32le                  -- ariProjectFPS                        0x01A4-0x01A7
    <*> getAriTimeCode128                              -- ariMasterTC                          0x01A8-0x01B7
    <*> getAriTimeCode128                              -- ariExtLTCFreeRun                     0x01B8-0x01C7 
    <*> getAriTimeCode128                              -- ariVITCFreeRun                       0x01C8-0x01D7 
    <*> getAriTimeCode128                              -- ariIntFreeRunToDTC                   0x01D8-0x01E7 
    <*> getAriTimeCode128                              -- ariIntFreeRunUDDTC                   0x01E8-0x01F7
    <*> getAriTimeCode128                              -- ariRecRunEdgeCodeTC                  0x01F8-0x0207
    <*> getAriTimeCode128                              -- ariRecRunClipCodeTC                  0x0208-0x0217
    <*> (getAriTimeCode128                             -- ariRecRunRegenTC                     0x0218-0x0227
    <*  skip 64)                                        -- Reserved                             0x0228-0x0267
    <*> getByteString 20                                -- ariMagazineSN                        0x0268-0x0267 
                                                        -- UMID : Unique material identifier
    <*> getByteString 32                                -- ariSMPTEUMID                         0x027C-0x029B 
    <*> getAriString 8                                 -- ariCameraType                        0x029C-0x02A3 
    <*> getAriString 32                                -- ariRecorderType                      0x02A4-0x02C3 
    <*> getByteString 1                                 -- ariMirrorShutterRun                  0x02C4-0x02C5 ???
    <*> (getByteString 1                                -- ariVariFrame                         0x02C5-0x02C6 ??
    <*  skip 2)                                         -- Reserved                             0x02C6-0x02C7
    <*> getByteString 4                                 -- ariExposurePhase                     0x02C8-0x02CB 
    <*> getByteString 4                                 -- ariSubSecondFrameCnt                 0x02CC-0x02CF 
    <*> getByteString 16                                -- ariUUID                              0x02D0-0x02DF 
    <*> (getAriString 24                               -- ariSUPVersion                        0x02E0-0x02F7 
    <*  skip 120)                                       -- Reserved                             0x02F8-0x036F

