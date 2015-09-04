-- |
-- Module      : ifstat.hs
-- Copyright   : Nathan Ingle
-- Licence     : BSD2
-- Maintainer  : elgni.nahtan@gmail.com
-- Stablility  : experimental
-- Portability : non-portable
--
-- Print network interface traffic statistics.
--

module Main where

import Network.Pcap
import Network.Pcap.Conduit
import Network.Pcap.Conduit.SourceLiveForever
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.EndOnQ

preambleEtc :: Integer
preambleEtc = 24

printAndReturnPkts :: [Packet] -> IO [Packet]
printAndReturnPkts ps = putBpsStr >> return ps
	where
		putBpsStr = putStrLn $ show (hdrSeconds (fst (head ps))) ++ ": " ++ show bps
		bps = sum $ map ((preambleEtc +) . toInteger . hdrWireLength . fst) ps

printBps :: Conduit Packet IO Packet
printBps = CL.groupBy isSameSecond =$= CL.concatMapM printAndReturnPkts
	where isSameSecond (h1, _) (h2, _) = hdrSeconds h1 == hdrSeconds h2

sumSink :: Sink Packet IO Integer
sumSink = CL.map getHdrWireLength =$ CL.fold (+) 0
	where getHdrWireLength = toInteger . hdrWireLength . fst

main :: IO ()
main = do
	putStrLn "Press q to quit"
	total <- sourceLiveForever "eth0" 65535 True 500000 $$ endOnQ =$ printBps =$ sumSink
	putStrLn $ "Got " ++ show total ++ " bits"
