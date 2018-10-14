{-# LANGUAGE DisambiguateRecordFields #-}

module Network.SourceRCON.ServerPacket where

import Prelude()

import Control.Monad((>>=))
import Data.Binary.Get(Get)
import Data.Binary.Put(Put)
import Data.Bool(otherwise)
import Data.Either(Either(Left, Right))
import Data.Eq((==))
import Data.Function(($), (.))
import Data.Functor((<$>))
import Data.Int(Int32)
import Data.List((++))
import Data.String(String)
import Network.SourceRCON.RawPacket(RawPacket(RawPacket), body, id, ptype, ID, Body, getRawPacket, putRawPacket)
import Network.SourceRCON.Util.Size(checkPacketSize, packetSize32)
import Network.SourceRCON.Util.Type(packetTypeToString)
import Text.Show(Show, show)

data ServerPacket =
  ResponsePacket {id :: ID, body :: Body} |
  AuthResponsePacket {id :: ID, body :: Body} deriving Show

responsePacketType :: Int32
responsePacketType = 0

authResponsePacketType :: Int32
authResponsePacketType = 2

getServerPacket :: Get (Either String ServerPacket)
getServerPacket = (>>= fromRaw) . (>>= checkPacketSize) <$> getRawPacket
  where
    fromRaw :: RawPacket -> Either String ServerPacket
    fromRaw x@RawPacket{body=b, id=i, ptype=t}
      | t == responsePacketType = Right $ ResponsePacket i b
      | t == authResponsePacketType = Right $ AuthResponsePacket i b
      | otherwise = Left $ "packet is not of server type: "++show t++packetTypeToString t++" in "++show x

putServerPacket :: ServerPacket -> Put
putServerPacket (ResponsePacket i b) = putRawPacket $ RawPacket (packetSize32 b) i responsePacketType b
putServerPacket (AuthResponsePacket i b) = putRawPacket $ RawPacket (packetSize32 b) i authResponsePacketType b
