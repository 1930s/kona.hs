{-# LANGUAGE OverloadedStrings, DataKinds, LambdaCase #-}

module Main where

import Lib
import Utils
import Opt
import Config

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TMQueue
import qualified Control.Concurrent.Thread.Group as ThreadGroup

import System.IO hiding (hPutStr)
import System.Directory
import System.FilePath
import System.Console.AsciiProgress

import Data.Monoid
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy (hPutStr)

import Network.HTTP.Req hiding ((=:))
import Options.Applicative (execParser)

main :: IO ()
main = displayConsoleRegions $
  execParser opts >>= return . parseOpts >>= parallel >>= takeMVar >> return ()

parallel :: CrawlerConfig -> IO (MVar Bool)
parallel (CrawlerConfig options
                        maxWorker
                        config@(PostConfig _ outputFolder httpConfig)
         ) = do
  exSet <- prepare outputFolder

  total <- getTotal options
  pbar <- newProgressBar $ progressBar total

  tq <- getPosts httpConfig options exSet
  tg <- ThreadGroup.new
  term <- newEmptyMVar

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

prepare :: FilePath -> IO (ExclusionSet)
prepare output =
  createDirectoryIfMissing True output >> listDirectory output >>=
  return . map takeBaseName >>=
  atomically . mkExclusionSet

getTotal :: Option Https -> IO (Int)
getTotal options = do
  runReq (httpConfig 0 0) $
    reqTotal (https "konachan.com" /: "post.xml") options

getPosts :: HttpConfig -> Option Https -> ExclusionSet -> IO (TMQueue Post)
getPosts httpConfig option exSet = do
  tq <- atomically $ newTMQueue
  tg <- ThreadGroup.new
  forkIO $ loop 1 tq
  return tq
  where
    loop :: Int -> TMQueue Post -> IO ()
    loop page tq =
      (runReq httpConfig $
       reqPosts (https "konachan.com" /: "post.json")
                (option <> queryParam "page" (pure page))) >>= \case
        [] -> atomically $ closeTMQueue tq
        ps -> do
          atomically $ filterMember exSet ps >>= mapM_ (writeTMQueue tq)
          loop (page + 1) tq

downloadPost :: PostConfig -> ProgressBar -> Post -> IO ()
downloadPost (PostConfig kind output config) pbar p =
  (runReq config $ reqPost kind output p) >>= saveImage >> tick pbar

saveImage :: DownloadResult -> IO ()
saveImage (filepath, rawData) = do
  withBinaryFile filepath WriteMode $ \h -> do
    hSetBuffering h (BlockBuffering Nothing)
    hPutStr h rawData
