{- |
Module      : ProcessArgs.hs
Copyright   : (c) Nathan Ingle
Licence     : BSD2

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable (probably)

Process command line arguments.
-}
module ProcessArgs where

import System.Environment (getArgs)
import Data.Bits ((.&.))
import Network.Info

getIfaceName :: IO String
getIfaceName = do
	args <- getArgs
	ifaces <- getNetworkInterfaces
	let ifaceArgs = filter (flip elem (map name ifaces)) args in
		if (length ifaceArgs > 0)
			then return $ head ifaceArgs
			else return $ name (head (interestingIfaces ifaces))

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