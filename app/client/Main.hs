
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import Network.Socket hiding (sendAllTo,recv)
import Network.Socket.ByteString (sendAllTo,recv)
import Data.Monoid((<>))

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B

main :: IO ()
main = do
  host:port:message:_ <- getArgs
  withSocketsDo $ do
    serverAddr <- resolveServer host port
    localAddr <- resolveServer "0.0.0.0" port
    sock <- bind' localAddr
    sendAllTo sock (T.encodeUtf8 $ T.pack $ message) (addrAddress serverAddr)
    bs <- recv sock 1024
    close sock
    B.putStr bs
    B.putStrLn ""

      
resolveServer host port = do
  let hints = defaultHints { addrFlags = [AI_PASSIVE]
                           , addrSocketType = Datagram
                           }
  addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
  return addr

  
bind' addr = do
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress addr)
  return sock
  
