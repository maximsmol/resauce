module Network.SourceRCON.SplitHelperPacket where

import Prelude()

import Data.Bool(Bool)
import {-# SOURCE #-} Network.SourceRCON.RawPacket(RawPacket)

isSplitHelperPacket :: RawPacket -> Bool
