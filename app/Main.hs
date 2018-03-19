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
import System.Console.AsciiProgress

import Data.Monoid
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy (hPutStr)
import Data.Sequence

import Network.HTTP.Req
import Options.Applicative (execParser)

main :: IO ()
main = displayConsoleRegions $
  execParser opts >>= return . parseOpts >>= parallel >>= takeMVar >> return ()

createFolder :: FilePath -> IO ()
createFolder = createDirectoryIfMissing True

saveImage :: DownloadResult -> IO ()
saveImage (filepath, rawData) = do
  withBinaryFile filepath WriteMode $ \h -> do
    hSetBuffering h (BlockBuffering Nothing)
    hPutStr h rawData

getPosts :: Option Https -> IO [Post]
getPosts option =
  runReq (httpConfig 50 5) $
  reqPosts (https "konachan.com" /: "post.json") option

getPostsChan :: Option Https -> IO (TMQueue Post)
getPostsChan option = do
  tq <- atomically $ newTMQueue
  tg <- ThreadGroup.new
  forkIO $ loop 1 tq
  return tq
  where
    loop :: Int -> TMQueue Post -> IO ()
    loop page tq =
      getPosts (option <> "page" =: page) >>= \case
        [] -> atomically $ closeTMQueue tq
        ps -> do
          atomically $ mapM_ (writeTMQueue tq) ps
          loop (page + 1) tq

getTotal :: Option Https -> IO (Int)
getTotal options = do
  runReq (httpConfig 0 0) $
    reqTotal (https "konachan.com" /: "post.xml") options

downloadPost :: PostConfig -> ProgressBar -> Post -> IO ()
downloadPost (PostConfig kind output config) pbar p =
  (runReq config $ reqPost kind output p) >>= saveImage >> tick pbar

serial :: CrawlerConfig -> IO ()
serial (CrawlerConfig options _ config) = do
  total <- getTotal options
  pbar <- newProgressBar def
  ps <- getPosts options
  mapM_ (downloadPost config pbar) ps

parallel :: CrawlerConfig -> IO (MVar Bool)
parallel (CrawlerConfig options
                        maxWorker
                        config@(PostConfig _ outputFolder _)
         ) = do
  createFolder outputFolder
  total <- getTotal options
  pbar <- newProgressBar $ progressBar total
  tq <- getPostsChan options
  term <- newEmptyMVar
  tg <- ThreadGroup.new
  forkIO $ loop tq tg term pbar
  return term
  where
    loop tq tg term pbar = do
      p <- atomically $ readTMQueue tq
      case p of
        Just post -> do
          limitThreads maxWorker tg >>
            ThreadGroup.forkIO
              tg
              (downloadPost config pbar post)
          loop tq tg term pbar
        Nothing -> do
          ThreadGroup.wait tg
          complete pbar
          putMVar term True
    limitThreads limit tg =
      atomically (ThreadGroup.nrOfRunning tg) >>= \num ->
        when (num >= limit) $ ThreadGroup.waitN (limit `div` 2) tg
