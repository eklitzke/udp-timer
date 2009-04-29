import Network.Socket
import Control.Monad
import System.IO.Unsafe
import System.Time
import GHC.Conc

data Descriptor = Descriptor Int ClockTime
 
showDesc :: Descriptor -> String
showDesc (Descriptor c t) = "Descriptor " ++ (show c) ++ " " ++ (show t)

-- what port the service listens on
servicePort = 9900

-- how much data we can recv
bufferSize = 2000

-- ten seconds
td :: TimeDiff
td = TimeDiff 0 0 0 0 0 10 0

-- global TVars
descCounter :: TVar Int
descCounter = unsafePerformIO $ newTVarIO 1 

activeCounterList :: TVar [Descriptor]
activeCounterList = unsafePerformIO $ newTVarIO []

-------

main :: IO ()
main = do
  forkIO reapForever
  withSocketsDo $ do
         sock <- socket AF_INET Datagram 0
         bindSocket sock (SockAddrInet servicePort iNADDR_ANY)
         (forever . processSocket) sock

-- Atomically increments descCounter, and adds a Descriptor to activeCounterList
newUdpCounter :: IO Int
newUdpCounter = atomically $ do
  oldVal <- readTVar descCounter
  timeNow <- unsafeIOToSTM $ getClockTime -- naughty
  writeTVar descCounter (oldVal + 1)
  cl <- readTVar activeCounterList
  let cl' = (Descriptor oldVal timeNow) : cl
  writeTVar activeCounterList cl'
  return oldVal

-- reap old counters
reapCounters :: ClockTime -> STM ()
reapCounters timeNow = do
  cl <- readTVar activeCounterList
  writeTVar activeCounterList (filter isYoung cl)
  where isYoung (Descriptor _ t) = diffClockTimes timeNow t < td

reapForever :: IO ()
reapForever = forever $ do
  timeNow <- getClockTime
  putStr $ "Woke up at " ++ (show timeNow)
  atomically $ reapCounters timeNow
  aclLen <- atomically $ readTVar activeCounterList
  putStrLn $ ", " ++ (show $ length aclLen) ++ " active counters"
  threadDelay (5 * 1000000)

-- process the socket
processSocket :: Socket -> IO ()
processSocket sock = do
  (mesg, len, client) <- recvFrom sock bufferSize
  newCounterVal <- newUdpCounter
  cl <- atomically (readTVar activeCounterList)
  let newMesg = show newCounterVal
  sendCount <- sendTo sock newMesg client
  return ()
