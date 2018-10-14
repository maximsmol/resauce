import Prelude(
  IO, Bool(True, False), String, Maybe(Just, Nothing), Either(Left, Right),
  (==), (++), (&&),
  ($), (.), (<$>), (>>), (>>=),
  fail, return, show, read, take)

import System.IO(Handle, hClose, hFlush, stdout, putStr, putStrLn, getLine)
import System.Random(randomIO)
import System.Environment(getArgs)
import System.Console.Haskeline(runInputT, defaultSettings, setComplete, getInputLine, InputT, noCompletion)

import Control.Monad(unless)
import Control.Monad.IO.Class(liftIO)

import Data.Int(Int32)
import Data.List.Split(splitOn)

import Data.ByteString.Lazy(null)
import Data.ByteString.Lazy.Char8(empty, pack, unpack)

import Network(connectTo, PortID(PortNumber))

import Network.SourceRCON.ClientPacket(ClientPacket(AuthPacket, ExecPacket))
import Network.SourceRCON.ServerPacket(ServerPacket(ResponsePacket, AuthResponsePacket))
import Network.SourceRCON.Handle(
  hGetServerPacket, hPutClientPacket, hPutServerPacket)

putFlush :: String -> IO ()
putFlush s = putStr s >> hFlush stdout

parseArgs :: [String] -> (Maybe String, Maybe String)
parseArgs [] = (Nothing, Nothing)
parseArgs [addr] = (Just addr, Nothing)
parseArgs [addr, pass] = (Just addr, Just pass)
parseArgs xs = parseArgs $ take 2 xs

maybeAskParam :: String -> Maybe String -> IO String
maybeAskParam _ (Just x) = return x
maybeAskParam prompt Nothing = do
  putFlush prompt
  getLine

attemptAuth :: String -> Handle -> IO Bool
attemptAuth pass sock = do
  randId <- randomIO
  hPutClientPacket sock $ AuthPacket randId (pack pass)
  res0 <- hGetServerPacket sock
  res0ok <- case res0 of
    Right (ResponsePacket id body) | id == randId && null body -> return True
    Left msg -> fail ("invalid auth response: "++msg) >> return False
    Right x -> fail ("unexpected auth response: "++show x) >> return False

  if res0ok then do
    res1 <- hGetServerPacket sock
    case res1 of
      Right (AuthResponsePacket id body) | id == -1 && null body -> return False
      Right (AuthResponsePacket id body) | id == randId && null body -> return True
      Left msg -> fail ("invalid auth response: "++msg) >> return False
      Right x -> fail ("unexpected auth response: "++show x) >> return False
  else
    return False

-- stack exec resauce 104.238.182.151:27025 willnowlayegg

loopAuth :: Handle -> IO ()
loopAuth sock = maybeAskParam "password: " Nothing >>= loopAuth' sock

loopAuth' :: Handle -> String -> IO ()
loopAuth' sock pass = do
  authOk <- attemptAuth pass sock
  unless authOk $ do
    putStrLn "wrong password"
    loopAuth sock

usageStr :: String
usageStr = "resauce [ip[:port]] [password]"

main :: IO ()
main = do
  putStrLn "resauce v0.1.0"
  (addrArg, passArg) <- parseArgs <$> getArgs
  addr <- maybeAskParam "address: " addrArg
  pass <- maybeAskParam "password: " passArg

  let (host, port) = case splitOn ":" addr of
        [h, p] -> (h, p)
        _ -> fail usageStr

  sock <- connectTo host $ PortNumber (read port)

  loopAuth' sock pass

  runInputT (setComplete noCompletion defaultSettings) $ repl sock

  hClose sock

repl :: Handle -> InputT IO ()
repl sock = do
  line <- getInputLine "> "
  case line of
    Just str -> do
      liftIO $ eval sock str
      repl sock
    _ -> return ()

loopResponse :: Int32 -> Int32 -> Handle -> IO Bool
loopResponse randId0 randId1 sock = do
  res0 <- hGetServerPacket sock
  case res0 of
    Right (ResponsePacket id body) | id == randId1 && null body -> return True
    Right (ResponsePacket id body) | id == randId0 -> do
      putStr . unpack $ body
      loopResponse randId0 randId1 sock
    _ -> return False

eval :: Handle -> String -> IO ()
eval sock cmd = do
  randId0 <- randomIO
  randId1 <- randomIO
  hPutClientPacket sock $ ExecPacket randId0 (pack cmd)
  hPutServerPacket sock $ ResponsePacket randId1 empty

  res0ok <- loopResponse randId0 randId1 sock
  if res0ok then do
    res1 <- hGetServerPacket sock
    res1ok <- case res1 of
      Right (ResponsePacket id body) | id == randId1 && null body -> return True
      _ -> return False

    unless res1ok .
      fail $ "invalid response to command: res1 not ok: " ++ show randId1 ++ " " ++ show randId0 ++ " " ++ show res1
  else
    fail $ "invalid response to command: res0 not ok: " ++ show randId1 ++ " " ++ show randId0 ++ " " ++ show res0ok
