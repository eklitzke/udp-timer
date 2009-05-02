module UdpTimer.Util where
import System.Time
import Network.Socket
import Control.Monad
import Control.Concurrent.Chan

import UdpTimer.Globals

-- ten seconds
reapInterval :: TimeDiff
reapInterval = TimeDiff 0 0 0 0 0 10 0

-- what port the service listens on
servicePort :: PortNumber
servicePort = 9900

-- how much data we can recv
bufferSize :: Int
bufferSize = 2000

-- create a socket and run processSocket forever 
doSocketLoop :: Chan (String, SockAddr) -> IO ()
doSocketLoop incoming = withSocketsDo $ do
                          sock <- socket AF_INET Datagram 0
                          bindSocket sock (SockAddrInet servicePort iNADDR_ANY)
                          forever $ do (mesg, len, client) <- recvFrom sock bufferSize
                                       writeChan incoming (mesg, client)
