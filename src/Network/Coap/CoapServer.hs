module Network.Coap.CoapServer where

import Control.Exception (catch, AsyncException(UserInterrupt))

import qualified Data.ByteString.Lazy as BS
import Data.Coap

import Network.Socket hiding (recvFrom)
import Network.Socket.ByteString

startCoapServer :: IO ()
startCoapServer = do
  sock <- socket AF_INET Datagram defaultProtocol
  addr <- inet_addr "127.0.0.1"
  bind sock (SockAddrInet 5683 addr)
  catch (coapThread sock) (\(UserInterrupt) -> close sock)
  close sock

coapThread :: Socket -> IO ()
coapThread sock = do
  (msg, addr) <- recvFrom sock 1280
  print $ parseMessage (BS.fromChunks [msg])
  coapThread sock
