{-# LANGUAGE DisambiguateRecordFields #-}

module Network.SourceRCON.ClientPacket where

import Prelude()

import Control.Monad((>>=))
import Data.Binary.Get(Get)
import Data.Binary.Put(Put)
import Data.Bool(otherwise)
import Data.Either(Either(Left, Right))
import Data.Eq((==))
import Data.Function(($))
import Data.Functor((<$>))
import Data.Int(Int32)
import Data.List((++))
import Data.String(String)
import Network.SourceRCON.RawPacket(RawPacket(RawPacket), id, body, ptype, ID, Body, getRawPacket, putRawPacket)
import Network.SourceRCON.Util.Size(packetSize32)
import Network.SourceRCON.Util.Type(packetTypeToString)
import Text.Show(Show, show)

data ClientPacket =
  ExecPacket {id :: ID, body :: Body} |
  AuthPacket {id :: ID, body :: Body} deriving Show

execPacketType :: Int32
execPacketType = 2

authPacketType :: Int32
authPacketType = 3

getClientPacket :: Get (Either String ClientPacket)
getClientPacket = (>>= fromRaw) <$> getRawPacket
  where
    fromRaw :: RawPacket -> Either String ClientPacket
    fromRaw x@RawPacket{id=i, body=b, ptype=t}
      | t == execPacketType = Right $ ExecPacket i b
      | t == authPacketType = Right $ AuthPacket i b
      | otherwise = Left $ "packet is not of client type: "++show t++packetTypeToString t++" in "++show x

putClientPacket :: ClientPacket -> Put
putClientPacket (ExecPacket i b) = putRawPacket $ RawPacket (packetSize32 b) i execPacketType b
putClientPacket (AuthPacket i b) = putRawPacket $ RawPacket (packetSize32 b) i authPacketType b
