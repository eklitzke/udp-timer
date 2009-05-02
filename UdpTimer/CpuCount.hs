-- Works only on Linux
module UdpTimer.CpuCount where

import System.IO

startsWith :: String -> String -> Bool
startsWith thing prefix = take (length prefix) thing == prefix

getCpuCount :: IO Int
getCpuCount = do cpuinfo <- openFile "/proc/cpuinfo" ReadMode
                 contents <- hGetContents cpuinfo
                 return $ length $ filter (\x -> x `startsWith` "processor") $ lines contents

