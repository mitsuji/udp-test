{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import Control.Exception (catch)
import Control.Monad (forever)
import Network.Socket hiding (recvFrom)
import Network.Socket.ByteString (recvFrom)
import GHC.IO.Handle.FD(stdout)
import Data.Monoid((<>))

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB

main :: IO ()
main = do
  host:port:_ <- getArgs
  withSocketsDo $ do
    addr <- resolveServer host port
    conn <- bind' addr
    forever $ (f conn) `catch` h

  where
    f :: Socket -> IO ()
    f conn = do
      (msg,caddr) <- recvFrom conn 64
      BB.hPutBuilder stdout $ BB.byteString msg <> "\n"


    h :: IOError -> IO ()
    h e = BB.hPutBuilder stdout $ "[EOF]" <> "\n"
      
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
  
