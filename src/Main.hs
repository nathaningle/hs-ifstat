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

import System.Environment (getArgs)
import Network.Pcap.Conduit.PrintBps
import Data.Conduit
import Data.Conduit.EndOnQ

import ProcessArgs

main :: IO ()
main = do
	args <- getArgs
	validateArgs args
	total <- (sourceFromArgs args) $$ endOnQ =$ printBps =$ sumSink
	putStrLn $ "Got " ++ show total ++ " bits"