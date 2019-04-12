{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import Network.Socket hiding (sendAllTo)
import Network.Socket.ByteString (sendAllTo)
import Data.Monoid((<>))

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

main :: IO ()
main = do
  host:port:message:_ <- getArgs
  withSocketsDo $ do
    addr <- resolveServer host port
    sock <- open' addr
    sendAllTo sock (T.encodeUtf8 $ T.pack message) (addrAddress addr)

      
resolveServer host port = do
  let hints = defaultHints { addrFlags = [AI_PASSIVE]
                           , addrSocketType = Datagram
                           }
  addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
  return addr


open' addr = do
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  return sock
  
