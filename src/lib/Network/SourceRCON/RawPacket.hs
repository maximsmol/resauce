module Network.SourceRCON.RawPacket where

import Prelude()

import Control.Applicative((<*>))
import Control.Monad((>>=), return)
import Data.Binary.Get(Get, getWord8, getInt32le, getLazyByteStringNul)
import Data.Binary.Put(Put, putInt32le, putLazyByteString, putWord8)
import Data.ByteString.Lazy(ByteString)
import Data.Either(Either(Left, Right))
import Data.Eq((/=))
import Data.Function(($))
import Data.Functor((<$>), (<$))
import Data.Int(Int32)
import Data.List((++))
import Data.String(String)
import Data.Word(Word8)
import Network.SourceRCON.SplitHelperPacket(isSplitHelperPacket)
import Text.Show(Show, show)

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
  } deriving Show

getRawPacket :: Get (Either String RawPacket)
getRawPacket = do
  res <- RawPacket <$> getInt32le <*> getInt32le <*> getInt32le <*> getLazyByteStringNul

  -- 4 byte body
  checks <- do
    _ <- if isSplitHelperPacket res then do
      let splitPacketMsg = "packet interpreted as a split helper has invalid payload ("++show res++")"
      _ <- assertByte' 1 splitPacketMsg
      _ <- assertByte' 0 splitPacketMsg
      _ <- assertByte' 0 splitPacketMsg
      return $ Right ()
    else
      return $ Right ()

    _ <- assertByte' 0 $ "terminator invalid for "++show res
    return $ Right ()

  return $ res <$ checks
  where
    assertByte' :: Word8 -> String -> Get (Either String ())
    assertByte' x msg = (/= x) <$> getWord8 >>= \r -> return $ if r then Right () else Left msg

putRawPacket :: RawPacket -> Put
putRawPacket (RawPacket s i t b) = do
  putInt32le s
  putInt32le i
  putInt32le t
  putLazyByteString b
  putWord8 0
  putWord8 0
