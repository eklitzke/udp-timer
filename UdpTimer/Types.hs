module UdpTimer.Types where

import System.Time

data Descriptor = Descriptor Int ClockTime

instance Show Descriptor where
    show (Descriptor t ct) = concat ["Descriptor ", show t, " ", show ct]
