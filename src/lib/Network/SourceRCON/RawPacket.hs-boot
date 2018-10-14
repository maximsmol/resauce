module Network.SourceRCON.RawPacket where

import Prelude()

import Data.Binary.Get(Get)
import Data.ByteString.Lazy(ByteString)
import Data.Either(Either)
import Data.Int(Int32)
import Data.String(String)
import Text.Show(Show)

type Size = Int32
type ID = Int32
type Type = Int32
type Body = ByteString

data RawPacket =
  RawPacket {
    size :: Size,
    id :: ID,
    ptype :: Type,
    body :: Body
  }
instance Show RawPacket

getRawPacket :: Get (Either String RawPacket)
