module Network.SourceRCON.SplitHelperPacket where

import Data.Bool(Bool, (&&))
import Data.ByteString.Lazy(null)
import Data.Eq((==))
import Network.SourceRCON.Util.Size(packetHeaderSize)
import Prelude((+))
import {-# SOURCE #-} Network.SourceRCON.RawPacket(RawPacket, size, body)

isSplitHelperPacket :: RawPacket -> Bool
isSplitHelperPacket x = (size x) == 4 + packetHeaderSize && null (body x)
