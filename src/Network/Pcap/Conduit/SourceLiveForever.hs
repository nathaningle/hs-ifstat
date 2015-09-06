{- |
Module      : Network.Pcap.Conduit.SourceLiveForever
Copyright   : (c) Nathan Ingle
Licence     : BSD2

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable (probably)

Extends Austin Seipp's pcap-conduit:
https://github.com/thoughtpolice/pcap-conduit/
-}

module Network.Pcap.Conduit.SourceLiveForever (sourceLiveForever) where

import Network.Pcap.Conduit (Packet)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Conduit (Source, yield)
import Data.Int (Int64)
import Network.Pcap

-- | Create a conduit 'Source' from a live interface, don't give up on no input.
sourceLiveForever :: MonadIO m
           => String -- ^ Device name
           -> Int    -- ^ Snapshot length in bytes
           -> Bool   -- ^ Promiscuous mode?
           -> Int64  -- ^ Timeout (in microseconds)
           -> Source m Packet
sourceLiveForever n s p t = (liftIO $ openLive n s p t) >>= sourcePcapForever

sourcePcapForever :: MonadIO m => PcapHandle -> Source m Packet
sourcePcapForever h = do
  pkt@(hdr,_) <- liftIO (nextBS h)
  if (hdrCaptureLength hdr == 0)
    then return () >> sourcePcapForever h
    else yield pkt >> sourcePcapForever h
