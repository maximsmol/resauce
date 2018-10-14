module Network.SourceRCON.Util.Size where

import Data.Bool((||))
import Data.ByteString.Lazy(ByteString, length)
import Data.Either(Either(Left, Right))
import Data.Eq((==))
import Data.Function(($))
import Data.Int(Int32, Int64)
import Data.List((++))
import Data.String(String)
import Prelude(Num, (+), fromIntegral)
import Text.Show(show)
import {-# SOURCE #-} Network.SourceRCON.RawPacket(RawPacket, size, body)
import {-# SOURCE #-} Network.SourceRCON.SplitHelperPacket(isSplitHelperPacket)

packetHeaderSize :: Num a => a
packetHeaderSize = 10

packetSize :: ByteString -> Int64
packetSize bs = packetHeaderSize + length bs

packetSize32 :: ByteString -> Int32
packetSize32 bs = fromIntegral $ packetSize bs

checkPacketSize :: RawPacket -> Either String RawPacket
checkPacketSize x =
  if isSplitHelperPacket x || (size x) == packetSize32 (body x) then
    Right x
  else
    Left $ "packet size invalid: "++show (size x)++" /= "++show (packetSize32 (body x))++" in "++show x
