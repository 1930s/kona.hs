{-# LANGUAGE OverloadedStrings, DataKinds, LambdaCase #-}

module Main where

import Lib
import Utils

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TMQueue
import qualified Control.Concurrent.Thread.Group as ThreadGroup

import System.IO hiding (hPutStr)
import System.Directory

import Data.Monoid
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy (hPutStr)
import Data.Sequence

import Network.HTTP.Req
import Options.Applicative (execParser)

main :: IO ()
main =
  execParser opts >>= return . parseOpts >>=
  parallel >>= takeMVar >>
  return ()

createFolder :: FilePath -> IO ()
createFolder = createDirectoryIfMissing True

saveImage :: DownloadResult -> IO ()
saveImage (filepath, rawData) = do
  withBinaryFile filepath WriteMode $ \h -> do
    hSetBuffering h (BlockBuffering Nothing)
    hPutStr h rawData

getPosts :: Option Https -> IO [Post]
getPosts option =
  runReq httpConfig $
  reqPosts (https "konachan.com" /: "post.json") option

getPostsChan :: Option Https -> IO (TMQueue Post)
getPostsChan option = do
  tq <- atomically $ newTMQueue
  forkIO (loop 1 tq)
  return tq
  where
    loop :: Int -> TMQueue Post -> IO ()
    loop page tq =
      getPosts (option <> "page" =: page) >>= \case
        [] -> atomically $ closeTMQueue tq
        ps -> do
          forkIO $ loop (page + 1) tq
          atomically $ mapM_ (writeTMQueue tq) ps

downloadPost :: FilePath -> Post -> IO ()
downloadPost f p = (runReq httpConfig $ reqPost f p) >>= saveImage

serial :: IO ()
serial =
  getPosts (mkParams [tags ["touhou", rating "safe"]]) >>=
  (mapM_ $ downloadPost "images")

parallel :: (Option Https, Int, FilePath)-> IO (MVar Bool)
parallel (options, maxWorker, outputFolder) = do
  createFolder outputFolder
  tq <- getPostsChan options
  term <- newEmptyMVar
  forkIO $ loop tq term
  return term
  where
    loop tq term = do
      tg <- ThreadGroup.new
      p <- atomically $ readTMQueue tq
      case p of
        Just post -> do
          limitThreads maxWorker tg >>
            ThreadGroup.forkIO tg (downloadPost outputFolder post)
          loop tq term
        Nothing -> do
          ThreadGroup.wait tg
          putMVar term True
    limitThreads limit tg =
      atomically (ThreadGroup.nrOfRunning tg) >>= \num ->
        when (num >= limit) $ ThreadGroup.waitN (limit `div` 2) tg
