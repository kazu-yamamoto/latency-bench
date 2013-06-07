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
  when (BS.length bs > 0) $ do
    NBS.send sock bs
    echo sock

server :: IO ()
server = do
    (addr:_) <- getAddrInfo (Just (defaultHints { addrFlags = [AI_PASSIVE] }))
                            Nothing
                            (Just ECHO_PORT)
    listenSock <- socket (addrFamily addr) Stream defaultProtocol
    setSocketOption listenSock ReuseAddr 1
    bindSocket listenSock (addrAddress addr)

    listen listenSock MAX_BACKLOG
    (sock, _) <- accept listenSock
    setSocketOption sock NoDelay 1

    echo sock

main :: IO ()
main = do
    putStrLn "Echo server started."
    runInUnboundThread server
    putStrLn "Echo server finished."
