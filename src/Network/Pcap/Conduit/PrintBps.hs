{- |
Module      : Network.Pcap.Conduit.PrintBps
Copyright   : (c) Nathan Ingle
Licence     : BSD2

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable (probably)

Print, every second, the number of bits seen.
-}

module Network.Pcap.Conduit.PrintBps (printBps, sumSink) where

import Network.Pcap
import Network.Pcap.Conduit
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Lift (evalStateC)
import Data.Word (Word32)
import Control.Monad.State

-- Note that pcap doesn't/can't capture Ethernet preamble, start frame delimiter,
-- frame check sequence or interpacket gap.  These total 24 bytes, which we
-- should take into account if we're measuring interface utilisation.
getPacketBits :: Packet -> Integer
getPacketBits (h, _) = 8 * packetBytes
	where
		packetBytes = preambleEtc + (toInteger $ hdrWireLength h)
		preambleEtc = 24

printPacketGroup :: [Packet] -> IO ()
printPacketGroup ps = putBpsStr
	where
		putBpsStr = putStrLn $ show (hdrSeconds (fst (head ps))) ++ ": " ++ show bps
		bps = sum $ map getPacketBits ps

printZeroSecs :: Word32 -> Word32 -> IO ()
printZeroSecs 0 _ = return ()
printZeroSecs a b = mapM_ (\s -> putStrLn (show s ++ ": 0")) [(a + 1) .. (b - 1)]

printPacketGroups :: MonadIO m => Conduit [Packet] m [Packet]
printPacketGroups = evalStateC 0 $ awaitForever $ \thisPktGrp -> do
	lastTS <- get
	let thisTS = hdrSeconds . fst $ head thisPktGrp
	put thisTS
	liftIO $ printZeroSecs lastTS thisTS
	liftIO $ printPacketGroup thisPktGrp
	yield thisPktGrp

printBps :: Conduit Packet IO Packet
printBps = CL.groupBy isSameSecond =$= printPacketGroups =$= CL.concat
	where isSameSecond (h1, _) (h2, _) = hdrSeconds h1 == hdrSeconds h2

sumSink :: Sink Packet IO Integer
sumSink = CL.map getPacketBits =$ CL.fold (+) 0