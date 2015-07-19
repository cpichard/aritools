{-# LANGUAGE BangPatterns #-}
module Codec.Picture.Ari.Header.CID where

-- CID Clip Information Data 

import Data.Binary
import Data.Binary.Get
import Control.Applicative
import Control.Monad
import Codec.Picture.Ari.Types

-- |Ari header data structure
data AriHeaderCIDv8 =
  AriHeaderCIDv8
    { ariHeaderCIDValid     :: !Int
    , ariCircleTake         :: !Int
    , ariReelName           :: !AriString
    , ariSceneName          :: !AriString
    , ariTakeName           :: !AriString
    , ariDirector           :: !AriString
    , ariCinematographer    :: !AriString
    , ariProductionName     :: !AriString
    , ariCompany            :: !AriString
    , ariNotes              :: !AriString
    , ariCameraClipName     :: !AriString
    } 

-- |Print functions
instance Show AriHeaderCIDv8 where
  show a = "CID valid: " ++ show (ariHeaderCIDValid a)
         ++ "\nreel name: " ++ show (ariReelName a)
         ++ "\ntake: " ++ show (ariTakeName a)
         ++ "\ncircle take: " ++ show (ariCircleTake a)
         ++ "\ndirector: " ++ show (ariDirector a)
         ++ "\ncinematographer: " ++ show (ariCinematographer a)
         ++ "\nproduction: " ++ show (ariProductionName a)
         ++ "\ncompany: " ++ show (ariCompany a)
         ++ "\nnotes: " ++ show (ariNotes a)
         ++ "\ncamera clip name: "++ show (ariCameraClipName a)

-- |Binary layout
decodeAriHeaderCIDv8 :: Get AriHeaderCIDv8
decodeAriHeaderCIDv8 = 
  AriHeaderCIDv8
    <$> liftM fromIntegral getWord32le                  -- ariCIDValid          0x04F0-0x04F3
    <*> liftM fromIntegral getWord32le                  -- ariCircleTake        0x04F4-0x04F7
    <*> getAriString 8                                 -- ariReelName          0x04F8-0x04FF 
    <*> getAriString 16                                -- ariSceneName         0x0500-0x050F
    <*> getAriString 8                                 -- ariTakeName          0x0510-0x0517
    <*> getAriString 32                                -- ariDirector          0x0518-0x0537 
    <*> getAriString 32                                -- ariCinematographer   0x0538-0x0557 
    <*> getAriString 32                                -- ariProductionName    0x0558-0x0577 
    <*> getAriString 32                                -- ariCompany           0x0578-0x0597 
    <*> getAriString 256                               -- ariNotes             0x0598-0x0697
    <*> (getAriString 24                               -- ariCameraClipName    0x0698-0x06B0 -> 25 ??
    <* skip 108)                                        -- Reserved             


