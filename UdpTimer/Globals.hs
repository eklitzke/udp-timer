module UdpTimer.Globals (descCounter, activeCounterList) where

import System.IO.Unsafe
import GHC.Conc
import UdpTimer.Util

descCounter :: TVar Int
descCounter = unsafePerformIO $ newTVarIO 1 

activeCounterList :: TVar [Descriptor]
activeCounterList = unsafePerformIO $ newTVarIO []
