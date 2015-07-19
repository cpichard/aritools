{-# LANGUAGE BangPatterns #-}
module Codec.Picture.Ari.Header.IDI where

-- IDI stands for Image Data Information

import Data.Binary
import Data.Binary.Get
import Control.Applicative
import Control.Monad

-- |Ari header data structure
--  Image Data Information part
data AriHeaderIDIv8 =
  AriHeaderIDIv8
    { ariIDIValid           :: !Int -- size of valid IDI data, should be 60
    , ariWidth              :: !Int
    , ariHeight             :: !Int
    , ariDataType           :: !Int
    , ariDataSpace          :: !Int
    , ariActiveImageX       :: !Int
    , ariActiveImageY       :: !Int
    , ariActiveImageW       :: !Int
    , ariActiveImageH       :: !Int 
    , ariFullImageX         :: !Int
    , ariFullImageY         :: !Int
    , ariFullImageW         :: !Int
    , ariFullImageH         :: !Int 
    , ariImageDataOffset    :: !Int
    , ariImageDataSize      :: !Int
    }

-- |Print functions
instance Show AriHeaderIDIv8 where
  show a = "IDI valid: "                 ++ show (ariIDIValid a)
        ++ "\nwidth: "                   ++ show (ariWidth a)
        ++ "\nheight: "                  ++ show (ariHeight a)
        ++ "\nactive image X: "          ++ show (ariActiveImageX a)
        ++ "\nactive image Y: "          ++ show (ariActiveImageY a)
        ++ "\nactive image W: "          ++ show (ariActiveImageW a)
        ++ "\nactive image H: "          ++ show (ariActiveImageH a)
        ++ "\nfull image X: "            ++ show (ariFullImageX a)
        ++ "\nfull image Y: "            ++ show (ariFullImageY a)
        ++ "\nfull image W: "            ++ show (ariFullImageW a)
        ++ "\nfull image H: "            ++ show (ariFullImageH a)
        ++ "\ndata offset: "             ++ show (ariImageDataOffset a)
        ++ "\ndata size: "               ++ show (ariImageDataSize a)


-- |Binary layout
decodeAriHeaderIDIv8 :: Get AriHeaderIDIv8
decodeAriHeaderIDIv8 = 
  AriHeaderIDIv8
    <$> liftM fromIntegral getWord32le                  -- ariIDIValid          Valid           0x0010-0x0013
    <*> liftM fromIntegral getWord32le                  -- ariWidth             Width           0x0014-0x0017
    <*> liftM fromIntegral getWord32le                  -- ariHeight            Height          0x0018-0x001B
    <*> liftM fromIntegral getWord32le                  -- ariDataType          DataType        0x001C-0x001F
    <*> liftM fromIntegral getWord32le                  -- ariDataSpace         DataSpace       0x0020-0x0023
    --                                                     ariActiveImage       ActiveImage     0x0024-0x0033
    <*> liftM fromIntegral getWord32le                  -- ariActiveImageX  
    <*> liftM fromIntegral getWord32le                  -- ariActiveImageY   
    <*> liftM fromIntegral getWord32le                  -- ariActiveImageW
    <*> liftM fromIntegral getWord32le                  -- ariActiveImageH
    --                                                     ariFullImage         FullImage       0x0034-0x0043
    <*> liftM fromIntegral getWord32le                  -- ariFullImageX  
    <*> liftM fromIntegral getWord32le                  -- ariFullImageY   
    <*> liftM fromIntegral getWord32le                  -- ariFullImageW
    <*> liftM fromIntegral getWord32le                  -- ariFullImageH
    <*> liftM fromIntegral getWord32le                  -- ariImageDataOffset   ImageDataOffset 0x0044-0x0047
    <*> (liftM fromIntegral getWord32le                 -- ariImageDataSize     DataSize        0x0048-0x004B
    <* skip 8)                                          -- Reserved                             0x004C-0x0053

