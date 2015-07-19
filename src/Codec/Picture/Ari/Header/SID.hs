{-# LANGUAGE BangPatterns #-}
module Codec.Picture.Ari.Header.SID where

-- SID 

import Data.Binary
import Data.Binary.Get
import Control.Applicative
import Control.Monad
import qualified Data.ByteString as BS
import Codec.Picture.Ari.Types

-- |Ari header data structure
data AriHeaderSIDv8 =
  AriHeaderSIDv8
    { ariSIDValid           :: !Int
    , ariSoundTC            :: !BS.ByteString
    , ariSoundFileName      :: !AriString
    , ariSoundRollName      :: !AriString
    , ariSoundSceneName     :: !AriString
    , ariSoundTakeName      :: !AriString
    , ariAudioInfo          :: !AriString
    , ariAudioDataOffset    :: !Int
    , ariAudioDataSize      :: !Int
    }

-- |Print functions
instance Show AriHeaderSIDv8 where
  show a = "SID valid: " ++ show (ariSIDValid a) 

-- |Binary layout
decodeAriHeaderSIDv8 :: Get AriHeaderSIDv8
decodeAriHeaderSIDv8 = 
  AriHeaderSIDv8
    <$> liftM fromIntegral getWord32be                  -- ariSIDValid          0x0718-0x071B
    <*> getByteString 16                                -- ariSoundTC           0x071C-0x072B 
    <*> getAriString 32                                -- ariSoundFileName     0x072C-0x074B 
    <*> getAriString 32                                -- ariSoundRollName     0x074C-0x076B 
    <*> getAriString 32                                -- ariSoundSceneName    0x076C-0x078B 
    <*> getAriString 32                                -- ariSoundTakeName     0x078C-0x07AB
    <*> getAriString 32                                -- ariAudioInfo         0x07AC-0x07CB 
    <*> liftM fromIntegral getWord32le                  -- ariAudioDataOffset   0x07CC-0x07CF
    <*> (liftM fromIntegral getWord32le                 -- ariAudioDataSize     0x07D0-0x07D3
    <*  skip 2092)                                      -- Reserved             0x07D4-0x0FFF

