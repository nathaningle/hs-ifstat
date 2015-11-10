{- |
Module      : ProcessArgs.hs
Copyright   : (c) Nathan Ingle
Licence     : BSD2

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable (probably)

Process command line arguments.
-}

module ProcessArgs (validateArgs, sourceFromArgs) where

import System.Exit (exitFailure, exitSuccess)
import Network.Info
import Network.Pcap.Conduit (sourceOffline, Packet)
import Network.Pcap.Conduit.SourceLiveForever
import Data.Bits ((.&.))
import Data.Conduit
import Data.Conduit.EndOnQ
import Control.Monad.IO.Class (liftIO, MonadIO)

mostInterestingIfaceName :: IO String
mostInterestingIfaceName = do
	ifaces <- getNetworkInterfaces
	return $ name (head (interestingIfaces ifaces))

interestingIfaces :: [NetworkInterface] -> [NetworkInterface]
interestingIfaces = filter (not . isBoring)
	where
		isBoring x = (ipv4 x == zeroIP) || isLoopbackIP (ipv4 x)
		zeroIP = IPv4 0x00000000  -- 0.0.0.0

isLoopbackIP :: IPv4 -> Bool
isLoopbackIP (IPv4 ip) = (ip .&. loopMask) == loopSubnet
	where
		loopSubnet = 0x0000007f  -- 127.0.0.0
		loopMask   = 0x000000ff  -- 255.0.0.0

-- Exit the program unless the arguments are valid.
validateArgs :: [String] -> IO ()
validateArgs args
	| length args == 0 = return ()
	| length args == 1 = case (head args) of
		"-h"     -> printUsageAndExit
		"--help" -> printUsageAndExit
		_        -> printUsageAndDie
	| length args == 2 = case (head args) of
		"-i"     -> return ()
		"-f"     -> return ()
		_        -> printUsageAndDie
	| otherwise = printUsageAndDie
	where
		printUsageAndExit = (putStrLn usageMsg) >> exitSuccess
		printUsageAndDie  = (putStrLn usageMsg) >> exitFailure
		usageMsg = "Usage: ifstat [-h | -i interface | -f pcap_file]"

-- You must run validateArgs before calling this, because the pattern matching
-- is non-exhaustive.
sourceFromArgs :: [String] -> Source IO Packet
sourceFromArgs args
	| length args == 0 = defaultSource
	| length args == 2 = case (head args) of
		"-i" -> liveSource (last args)
		"-f" -> sourceOffline (last args)
	where
		defaultSource = (liftIO mostInterestingIfaceName) >>= liveSource
		liveSource ifaceName = do
			liftIO $ putStrLn ("Using interface " ++ ifaceName)
			liftIO $ putStrLn "Press q to quit"
			sourceLiveForever ifaceName 65535 True 500000 $= endOnQ
