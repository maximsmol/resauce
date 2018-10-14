module Network.SourceRCON.ClientPacket where

import Data.Binary.Get(Get)
import Data.Int(Int32)
import Data.String(String)
import Network.SourceRCON.RawPacket(ID, Body)
import Text.Show(Show)

data ClientPacket =
  ExecPacket {id :: ID, body :: Body} |
  AuthPacket {id :: ID, body :: Body}
instance Show ClientPacket

execPacketType :: Int32
authPacketType :: Int32

getClientPacket :: Get (Either String ClientPacket)
