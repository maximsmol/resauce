{-# LANGUAGE DisambiguateRecordFields #-}

module Network.SourceRCON.ServerPacket where

import Prelude()

import Data.Binary.Get(Get)
import Data.Either(Either)
import Data.Int(Int32)
import Data.String(String)
import Network.SourceRCON.RawPacket(ID, Body)
import Text.Show(Show)

data ServerPacket =
  ResponsePacket {id :: ID, body :: Body} |
  AuthResponsePacket {id :: ID, body :: Body}
instance Show ServerPacket

responsePacketType :: Int32
authResponsePacketType :: Int32

getServerPacket :: Get (Either String ServerPacket)
