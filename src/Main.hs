{-# LANGUAGE NumericUnderscores #-}

module Main where

import           System.IO
import           Network.Socket                 ( Socket
                                                , socketToHandle
                                                )
import           Network.Run.TCP
import           Control.Concurrent             ( threadDelay )
import           Control.Exception              ( handle )
import           System.Random                  ( Random
                                                , randomIO
                                                )
import           Data.Word                      ( Word32 )
import           Text.Printf                    ( hPrintf )


handler :: Socket -> IO ()
handler sock =
  putStrLn ("New connection: " ++ show sock)
    >>  socketToHandle sock ReadWriteMode
    >>= loop
 where
  loop hSock = do
    threadDelay 10_000_000
    handle onErr $ do
      r <- randomIO :: IO Word32
      hPrintf hSock "%x\r\n" r
      loop hSock
  onErr :: IOError -> IO ()
  onErr _ = putStrLn $ "Connection " ++ show sock ++ " closed!"

main :: IO ()
main = runTCPServer (Just "0.0.0.0") "2222" handler
