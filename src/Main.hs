{- |
Module      : Main.hs
Copyright   : (c) Nathan Ingle
Licence     : BSD2

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable (probably)

Print network interface traffic statistics.
-}

module Main where

import Network.Pcap.Conduit.SourceLiveForever
import Network.Pcap.Conduit.PrintBps
import Data.Conduit
import Data.Conduit.EndOnQ

import ProcessArgs (getIfaceName)

main :: IO ()
main = do
	ifaceName <- getIfaceName
	putStrLn $ "Using interface " ++ ifaceName
	putStrLn "Press q to quit"
	total <- sourceLiveForever ifaceName 65535 True 500000 $$ endOnQ =$ printBps =$ sumSink
	putStrLn $ "Got " ++ show total ++ " bits"
