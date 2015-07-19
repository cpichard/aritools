{-# LANGUAGE BangPatterns #-}
module Codec.Picture.Ari.Header.LDI where

import Data.Binary
import Data.Binary.Get
import Data.Binary.IEEE754
import Control.Applicative
import Control.Monad
import qualified Data.ByteString as BS
import Codec.Picture.Ari.Types

-- |Ari header data structure
data AriHeaderLDIv8 =
  AriHeaderLDIv8
    { ariLDIValid           :: !Int
    , ariFocusUnit          :: !Int
    , ariFocusDistance      :: !Float -- lens focus distance type
    , ariFocalLength        :: !Float
    , ariLensSerialNumber   :: !Int
    -- , ariLinearIris         :: !Int
    --, ariIris               :: !Float
    , ariIris               :: !Int
    , ariNDFilter           :: !Int -- Split in filter type/Density
    , ariUVFilter           :: !Int
    , ariIRFilter           :: !Int
    , ariIRBlackFilter      :: !Int
    , ariLensModel          :: !AriString -- 32
    -- TODO : encode as Int ? or Short ?
    , ariRawEncoderFocus    :: !BS.ByteString -- 4 Lds & Motor
    , ariRawEncoderFocal    :: !BS.ByteString -- 4 Lds & Motor
    , ariRawEncoderIris     :: !BS.ByteString -- 4 Lds & Motor
    , ariLimitFocusLds      :: !BS.ByteString -- 4
    , ariLimitFocalLds      :: !BS.ByteString -- 4
    , ariLimitIrisLds       :: !BS.ByteString -- 4
    , ariLimitFocusMotor    :: !BS.ByteString -- 4
    , ariLimitFocalMotor    :: !BS.ByteString -- 4
    , ariLimitIrisMotor     :: !BS.ByteString -- 4
    , ariLdsLagType         :: !BS.ByteString -- 1
    , ariLdsLagValue        :: !BS.ByteString -- 1
    }

-- |Print functions
instance Show AriHeaderLDIv8 where
  show a = "LDI valid: " ++ show (ariLDIValid a)
        ++ "\nlens model: " ++ show (ariLensModel a) 
        ++ "\nfocus unit: " ++ show (ariFocusUnit a)
        ++ "\nfocus distance: " ++ show (ariFocusDistance a)
        ++ "\nfocal length: " ++ show (ariFocalLength a)
        ++ "\nlens serial number: " ++ show (ariLensSerialNumber a)
        ++ "\niris: " ++ show (ariIris a)
        ++ "\nneutral density filter: " ++ show (ariNDFilter a)
        -- ++ "\nuv filter: " ++ show (ariUVFilter a)
        -- ++ "\nir filter: " ++ show (ariIRFilter a)
        -- ++ "\nir black filter: " ++ show (ariIRBlackFilter a)

--arriHeaderLDIv8Offset = 0x0370

-- |Binary layout
decodeAriHeaderLDIv8 :: Get AriHeaderLDIv8
decodeAriHeaderLDIv8 = 
  AriHeaderLDIv8
    <$> liftM fromIntegral getWord32le -- ariLDIValid           0x0370-0x0373
    <*> liftM fromIntegral getWord32le -- ariFocusUnit          0x0374-0x0377
    <*> getFloat32le                   -- ariFocusDistance      0x0378-0x037B
    <*> getFloat32le                   -- ariFocalLength        0x037C-0x037F 
    <*> liftM fromIntegral getWord32le -- ariLensSerialNumber   0x0380-0x0383
    <*> liftM fromIntegral getWord32le -- ariIris               0x0384-0x0387
    <*> liftM fromIntegral getWord32le -- ariNDFilter           0x0388-0x038B
    <*> liftM fromIntegral getWord32le -- ariUVFilter           0x038C-0x038F 
    <*> liftM fromIntegral getWord32le -- ariIRFilter           0x0390-0x0393 
    <*> liftM fromIntegral getWord32le -- ariIRBlackFilter      0x0394-0x0397
    <*> getAriString 32               -- ariLensModel          0x0398-0x03B7
    <*> getByteString 4                -- ariRawEncoderFocus    0x03B8-0x03BB 
    <*> getByteString 4                -- ariRawEncoderFocal    0x03BC-0x03BF
    <*> getByteString 4                -- ariRawEncoderIris     0x03C0-0x03C3
    <*> getByteString 4                -- ariLimitFocusLds      0x03C4-0x03C7
    <*> getByteString 4                -- ariLimitFocalLds      0x03C8-0x03CB
    <*> getByteString 4                -- ariLimitIrisLds       0x03CC-0x03CF
    <*> getByteString 4                -- ariLimitFocusMotor    0x03D0-0x03D3 
    <*> getByteString 4                -- ariLimitFocalMotor    0x03D4-0x03D7
    <*> getByteString 4                -- ariLimitIrisMotor     0x03D8-0x03DB
    <*> getByteString 1                -- ariLdsLagType         0x03DC-0x03DC
    <*> (getByteString 1               -- ariLdsLagValue        0x03DD-0x03DD
    <*  skip 90)                       -- Reserved              0x03DC-0x0437 (error in the specs ?)



