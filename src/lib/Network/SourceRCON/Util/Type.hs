module Network.SourceRCON.Util.Type where

import Prelude()

import Data.Bool(otherwise)
import Data.Eq((==))
import Data.String(String)
import {-# SOURCE #-} Network.SourceRCON.ClientPacket(execPacketType, authPacketType)
import Network.SourceRCON.RawPacket(Type)
import {-# SOURCE #-} Network.SourceRCON.ServerPacket(responsePacketType, authResponsePacketType)

packetTypeToString :: Type -> String
packetTypeToString t
  | t == responsePacketType = "ResponsePacket"
  | t == authResponsePacketType = "AuthResponsePacket"
  | t == execPacketType = "ExecPacket"
  | t == authPacketType = "AtuhPacket"
  | otherwise = "[unknown]"
