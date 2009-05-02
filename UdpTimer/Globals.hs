module UdpTimer.Globals where

import System.IO.Unsafe
import GHC.Conc
import Control.Concurrent.Chan
import UdpTimer.Types

descCounter :: TVar Int
descCounter = unsafePerformIO $ newTVarIO 1 

activeCounterList :: TVar [Descriptor]
activeCounterList = unsafePerformIO $ newTVarIO []
