{-# LANGUAGE CPP, BangPatterns #-}

#include "echodef.h"

module Main where

import Control.Concurrent
import Control.Monad
import qualified Data.ByteString as BS
import Network.Socket
import qualified Network.Socket.ByteString as NBS

echo :: Socket -> IO ()
echo sock = do
  bs <- NBS.recv sock ECHO_SIZE
  if BS.length bs > 0 then do
      void $ NBS.send sock bs
      echo sock
    else
      sClose sock

loop :: Socket -> IO ()
loop listenSock = do
    (sock, _) <- accept listenSock
    setSocketOption sock NoDelay 1
    void . forkIO $ echo sock
    loop listenSock

server :: IO ()
server = do
    (addr:_) <- getAddrInfo (Just (defaultHints { addrFlags = [AI_PASSIVE] }))
                            Nothing
                            (Just ECHO_PORT)
    listenSock <- socket (addrFamily addr) Stream defaultProtocol
    setSocketOption listenSock ReuseAddr 1
    bindSocket listenSock (addrAddress addr)
    listen listenSock MAX_BACKLOG

    loop listenSock

main :: IO ()
main = do
    putStrLn "Echo server started."
    runInUnboundThread server
    -- never reached
    putStrLn "Echo server finished."
