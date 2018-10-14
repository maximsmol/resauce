module Network.SourceRCON.Handle where

import Data.Binary.Get(Get, runGet, getInt32le)
import Data.Binary.Put(Put, runPut)
import Prelude(fromIntegral)
import Data.ByteString.Lazy(hGet, hPut, append)
import Data.Either(Either)
import Data.Function((.), ($))
import Control.Monad(return)
import Data.String(String)
import Network.SourceRCON.ClientPacket(ClientPacket, getClientPacket, putClientPacket)
import Network.SourceRCON.ServerPacket(ServerPacket, getServerPacket, putServerPacket)
import System.IO(Handle, IO)

hPutPacket :: (a -> Put) -> Handle -> a -> IO ()
hPutPacket f sock = hPut sock . runPut . f

hPutClientPacket :: Handle -> ClientPacket -> IO ()
hPutClientPacket = hPutPacket putClientPacket

hPutServerPacket :: Handle -> ServerPacket -> IO ()
hPutServerPacket = hPutPacket putServerPacket


hGetPacket :: Get (Either String a) -> Handle -> IO (Either String a)
hGetPacket f sock = do
  sizeS <- hGet sock 4
  let psize = runGet getInt32le sizeS
  dataS <- hGet sock $ fromIntegral psize
  return . runGet f $ append sizeS dataS

hGetClientPacket :: Handle -> IO (Either String ClientPacket)
hGetClientPacket = hGetPacket getClientPacket

hGetServerPacket :: Handle -> IO (Either String ServerPacket)
hGetServerPacket = hGetPacket getServerPacket
