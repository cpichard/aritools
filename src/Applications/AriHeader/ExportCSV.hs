module Applications.AriHeader.ExportCSV where


import Data.List (intersperse)
import Codec.Picture.Ari.Header
import Codec.Picture.Ari.Header.CDI
import Codec.Picture.Ari.Header.CID
import Codec.Picture.Ari.Header.IDI
import Codec.Picture.Ari.Header.ICI
import Codec.Picture.Ari.Header.SID
import Codec.Picture.Ari.Header.VFX
import Codec.Picture.Ari.Header.LDI
import Numeric

showField :: Show a => (t1 -> t) -> (t -> a) -> t1 -> String
showField a b x = show (b (a x))

showFieldHex :: (Show a, Integral a) =>
                (t1 -> t) -> (t -> a) -> t1 -> String
showFieldHex a b x = showHex (b (a x)) ""

-- |Exported fields as in AriMetadataExtractor
exportedFields :: [([Char], AriHeader -> String)]
exportedFields = 
   [ ("Master TC", showField ariCDI ariMasterTC)
   , ("Reel", showField ariCID ariReelName)
   , ("Scene", showField ariCID ariSceneName)
   , ("Take", showField ariCID ariTakeName)
   , ("Director", showField ariCID ariDirector)
   , ("Cinematographer", showField ariCID ariCinematographer)
   , ("Production", showField ariCID ariProductionName)
   , ("Circle Take", showField ariCID ariCircleTake) 
   , ("Company", (\_->""))
   , ("Location", (\_-> "")) 
   , ("Operator", (\_-> ""))
   , ("UserInfo1", (\_ -> ""))
   , ("UserInfo2", (\_ -> ""))
   , ("Camera Clip Name", showField ariCID ariCameraClipName)
   , ("Camera Type", showField ariCDI ariCameraType)
   , ("Camera Type ID", showField ariCDI ariCameraTypeId)
   , ("Camera Serial Number", showField ariCDI ariCameraSN)
   , ("Camera ID", showField ariCDI ariCameraID)
   , ("Camera Index", showField ariCDI ariCameraIndex)
   --, ("Camera SUP Version", showField ariSUPVersion )    -- TODO which field is this one ?
   , ("Camera SUP Name", showField ariCDI ariSUPVersion) -- TODO which field is this one ? split with :
   , ("System Image Creation Date", showFieldHex ariCDI ariSysImgCreationTime) -- TODO date and time 
   , ("System Image Creation Time", showFieldHex ariCDI ariSysImgCreationTime) -- TODO date and time
   , ("Exposure Time", showField ariCDI ariExposureTime)
   , ("Shutter Angle", showField ariCDI ariShutterAngle)
   , ("Mirror Shutter Running", showField ariCDI ariMirrorShutterRun)
   , ("Sensor FPS", showField ariCDI ariSensorFPS)
   , ("Project FPS", showField ariCDI ariProjectFPS)
   , ("Master TC", showField ariCDI ariMasterTC)
   , ("Master TC Drop Frame", (\_->"")) -- TODO 
   , ("Master TC Time Base",  (\_->"")) -- TODO which field is this one ?
   , ("Master TC Frame Count", (\_->""))-- TODO which field is this one ?
   , ("Master TC User Info",  (\_->"")) -- TODO which field is this one ?
   , ("Storage Media Serial Number", showField ariCDI ariMagazineSN)
   , ("SMPTE UMID", showField ariCDI ariSMPTEUMID)
   , ("Recorder Type", showField ariCDI ariRecorderType)
   , ("Vari", (\_->""))
   , ("UUID", showField ariCDI ariUUID)
   , ("Image Width", showField ariIDI ariWidth)
   , ("Image Height", showField ariIDI ariHeight)
   , ("Active Image Width", showField ariIDI ariActiveImageW) 
   , ("Active Image Height", showField ariIDI ariActiveImageH)
   , ("Active Image Top", showField ariIDI ariActiveImageW)  -- TODO
   , ("Active Image Left", showField ariIDI ariActiveImageH) -- TODO
   , ("Color Processing Version", (\_->""))
   , ("White Balance", showField ariICI ariWhiteBalance) 
   , ("White Balance CC", showField ariICI ariWhiteLevel) -- TODO Double check field is correct
   , ("WB Factor R", (\_->"")) -- TODO 
   , ("WB Factor G", (\_->"")) -- TODO
   , ("WB Factor B", (\_->"")) -- TODO
   , ("WB Applied in Camera", showField ariICI ariWBAppliedInCamera)
   , ("Exposure Index ASA", showField ariICI ariExposureIndex)
   , ("Target Color Space", showField ariICI ariTargetColorSpace)
   , ("Sharpness", showField ariICI ariSharpness)
   , ("Aspect Ratio", showField ariICI ariPixelAspectRatio)
   , ("Image Flip", showField ariICI ariFlip)
   , ("Look", showField ariICI ariLookFile)
   , ("Look Burned In", (\_->"")) -- TODO 
   , ("Look LUT Mode",  (\_->"")) -- TODO
   , ("Look LUT Offset",(\_->"")) -- TODO
   , ("Look LUT Size",(\_->"")) -- TODO
   , ("Look LUT CRC Checksum", (\_->"")) -- TODO
   , ("Look Saturation", showField ariICI ariLinearSaturation) 
   , ("CDL Slope R", showField ariICI ariCdlSlopeR)
   , ("CDL Slope G", showField ariICI ariCdlSlopeG)
   , ("CDL Slope B", showField ariICI ariCdlSlopeB)
   , ("CDL Offset R", showField ariICI ariCdlOffsetR)
   , ("CDL Offset G", showField ariICI ariCdlOffsetG)
   , ("CDL Offset B", showField ariICI ariCdlOffsetB)
   , ("CDL Power R", showField ariICI ariCdlPowerR)
   , ("CDL Power G", showField ariICI ariCdlPowerG)
   , ("CDL Power B", showField ariICI ariCdlPowerB)
   -- TODO split printer lights in rgb components
   --,("Printer Lights R", showField ariICI ariPrinterLights)
   --, ("Printer Lights G", showField ariICI ariPrinterLights)
   --, ("Printer Lights B", showField ariICI ariPrinterLights)
   , ("Printer Lights R", (\_->"")) -- TODO
   , ("Printer Lights G", (\_->"")) -- TODO
   , ("Printer Lights B", (\_->"")) -- TODO
   , ("CDL Mode", (\_->"")) -- TODO
   , ("Lens Model", showField ariLDI ariLensModel)
   , ("Lens Serial Number", showField ariLDI ariLensSerialNumber)
   , ("Lens Focus Unit", showField ariLDI ariFocusUnit)
   , ("Lens Focus Distance", showField ariLDI ariFocusDistance)
   , ("Lens Focal Length", showField ariLDI ariFocalLength)
   , ("Lens Iris", showField ariLDI ariIris)
   , ("Lens Linear Iris", (\_->"")) 
   , ("ND Filter Type", showField ariLDI ariNDFilter) -- TODO type and density mismatch ?
   , ("ND Filter Density", showField ariLDI ariNDFilter)
   , ("Camera Tilt", showField ariVFX ariTilt)
   , ("Camera Roll", showField ariVFX ariRoll)
   , ("Sound Roll", showField ariSID ariSoundRollName)
   ]

-- |Encode fields in CSV 
exportCSV :: AriHeader -> String
exportCSV header = createHeader ++ "\n" ++ createFields
  where createHeader = concat $ intersperse "\t" [k | (k, _) <- exportedFields] 
        createFields = concat $ intersperse "\t" [(k header) | (_, k) <- exportedFields]
-- |
showFields :: AriHeader -> String
showFields header = concat $ intersperse "\n" $ map (\(x,y) -> (show x) ++ "\t" ++ (y header)) exportedFields


