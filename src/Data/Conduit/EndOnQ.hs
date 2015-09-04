-- |
-- Module      : Data.Conduit.EndOnQ
-- Copyright   : Nathan Ingle
-- Licence     : BSD2
-- Maintainer  : elgni.nahtan@gmail.com
-- Stablility  : experimental
-- Portability : non-portable
--
-- Provides a conduit to watch standard input and terminate upon receipt of 'q'.
--

module Data.Conduit.EndOnQ (endOnQ) where

import Data.Conduit
import Control.Monad.IO.Class (liftIO)
import System.IO

endOnQ :: Conduit a IO a
endOnQ = do
	liftIO $ hSetBuffering stdin NoBuffering
	liftIO $ hSetEcho stdin False
	
	keyPressed <- liftIO $ hReady stdin
	if keyPressed
		then do
			k <- liftIO getChar
			case k of
				'q' -> return ()
				_   -> keepGoing
		else keepGoing
	where keepGoing = do
		c <- await
		case c of
			Just c' -> yield c'
			Nothing -> return ()
		endOnQ
