module UdpTimer.Util where
import System.Time
import Network.Socket
import Control.Monad

data Descriptor = Descriptor Int ClockTime

instance Show Descriptor where
    show (Descriptor t ct) = concat ["Descriptor ", show t, " ", show ct]

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
doSocketLoop :: (Socket -> IO a) -> IO a
doSocketLoop processSocket = withSocketsDo $ do
                               sock <- socket AF_INET Datagram 0
                               bindSocket sock (SockAddrInet servicePort iNADDR_ANY)
                               forever $ processSocket sock
