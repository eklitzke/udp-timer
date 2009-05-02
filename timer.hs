import Network.Socket
import Control.Monad
import System.IO.Unsafe
import System.Time
import GHC.Conc
import Control.ThreadPool
import Text.Printf

import UdpTimer.Util
import UdpTimer.Globals
import UdpTimer.Types
import UdpTimer.CpuCount

main :: IO ()
main = do nrCpu <- getCpuCount
          let workerCount = nrCpu * 2
          printf "Spawning %d worker threads...\n" workerCount
          (input, output) <- threadPoolIO workerCount handleReq
          forkIO reapForever
          --forkIO $ doSocketLoop input
          doSocketLoop input

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
  where isYoung (Descriptor _ t) = diffClockTimes timeNow t < reapInterval

reapForever :: IO ()
reapForever = forever $ do
  timeNow <- getClockTime
  putStr $ "Woke up at " ++ (show timeNow)
  atomically $ reapCounters timeNow
  aclLen <- atomically $ readTVar activeCounterList
  putStrLn $ ", " ++ (show $ length aclLen) ++ " active counters"
  threadDelay (5 * 1000000)
              
handleReq :: (Socket, String, SockAddr) -> IO ()
handleReq (sock, mesg, client) = do
  case mesg of
    "NEW" -> do val <- newUdpCounter
                sendTo sock (show val) client
    _     -> sendTo sock "ERROR" client
  return ()

