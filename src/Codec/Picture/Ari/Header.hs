{-# LANGUAGE BangPatterns #-}
module Codec.Picture.Ari.Header where

import Data.Binary
import Data.Binary.Get
import Control.Applicative
import Data.List (intersperse)
import qualified Data.ByteString.Lazy as BL

import Codec.Picture.Ari.Header.RID
import Codec.Picture.Ari.Header.IDI
import Codec.Picture.Ari.Header.ICI
import Codec.Picture.Ari.Header.CDI
import Codec.Picture.Ari.Header.LDI
import Codec.Picture.Ari.Header.VFX
import Codec.Picture.Ari.Header.CID
import Codec.Picture.Ari.Header.SID

-- |Ari header data structure
data AriHeader =
  AriHeaderV8
    { ariRID :: !AriHeaderRID
    , ariIDI :: !AriHeaderIDIv8
    , ariICI :: !AriHeaderICIv8
    , ariCDI :: !AriHeaderCDIv8
    , ariLDI :: !AriHeaderLDIv8
    , ariVFX :: !AriHeaderVFXv8
    , ariCID :: !AriHeaderCIDv8
    , ariSID :: !AriHeaderSIDv8
    } 

-- |Print functions
instance Show AriHeader where
  show a = 
      let displayList = [ "===RID==="
                     , show (ariRID a)
                     , "===IDI==="
                     , show (ariIDI a)
                     , "===ICI==="
                     , show (ariICI a)
                     , "===CDI==="
                     , show (ariCDI a)
                     , "===LDI==="
                     , show (ariLDI a)
                     , "===VFX==="
                     , show (ariVFX a)
                     , "===CID==="
                     , show (ariCID a)
                     ]
      in concat $ intersperse "\n" displayList

-- |Binary layout
decodeAriHeaderV8 :: Get AriHeader
decodeAriHeaderV8 = 
  AriHeaderV8
    <$> decodeAriHeaderRID 
    <*> decodeAriHeaderIDIv8
    <*> decodeAriHeaderICIv8
    <*> decodeAriHeaderCDIv8
    <*> decodeAriHeaderLDIv8
    <*> decodeAriHeaderVFXv8
    <*> decodeAriHeaderCIDv8
    <*> decodeAriHeaderSIDv8

readHeader :: FilePath -> IO AriHeader
readHeader f = do
  input <- BL.readFile f
  return $ runGet decodeAriHeaderV8 input
