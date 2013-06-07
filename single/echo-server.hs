{-# LANGUAGE CPP, BangPatterns #-}

#include "echo.h"

module Main where

import Control.Concurrent
import Control.Monad
import qualified Data.ByteString as BS
import Network.Socket
import qualified Network.Socket.ByteString as NBS

main :: IO ()
main = do
    putStrLn "Echo server started."
    serverAddrs <- getAddrInfo
                   (Just (defaultHints { addrFlags = [AI_PASSIVE] } ))
                   Nothing
                   (Just ECHO_PORT)
    let serverAddr = head serverAddrs
    sock <- socket (addrFamily serverAddr) Stream defaultProtocol
    setSocketOption sock ReuseAddr 1
    setSocketOption sock NoDelay 1
    bindSocket sock (addrAddress serverAddr)

    listen sock 5
    (clientSock, _) <- accept sock

    runInUnboundThread $ pong clientSock
    putStrLn "Echo server finished."

pong :: Socket -> IO ()
pong sock = do
  bs <- NBS.recv sock 8
  -- putStrLn $ "server received " ++ unpack bs
  when (BS.length bs > 0) $ do
    NBS.send sock bs
    pong sock
