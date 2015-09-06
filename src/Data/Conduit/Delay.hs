{- |
Module      : Data.Conduit.Delay
Copyright   : (c) Nathan Ingle
Licence     : BSD2

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable (threadDelay requires GHC)

Provides a conduit to insert a delay.
-}
module Data.Conduit.Delay (delay) where

import Data.Conduit
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)

-- | Create a Conduit to insert a delay.
delay :: Int  -- ^ Delay (in microseconds)
      -> Conduit a IO a
delay usec = do
	mi <- await
	liftIO $ threadDelay usec
	case mi of
		Nothing -> return ()
		Just i -> do
			yield i
			delay usec
