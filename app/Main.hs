{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Config
import           Lib
import           Opt
import           Utils

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TMQueue
import qualified Control.Concurrent.Thread.Group
                                               as ThreadGroup
import           Control.Monad

import           System.Console.AsciiProgress
import           System.Directory
import           System.FilePath
import           System.IO               hiding ( hPutStr )

import           Data.ByteString                ( hPutStr )
import           Data.Monoid

import           Network.HTTP.Req        hiding ( getHttpConfig )
import           Options.Applicative            ( execParser )

main :: IO ()
main =
  displayConsoleRegions
    $   parseOpts
    <$> execParser opts
    >>= parallel
    >>= takeMVar
    >>  return ()

parallel :: CrawlerConfig -> IO (MVar Bool)
parallel (CrawlerConfig options maxWorker limit exclusion config@(PostConfig _ outputFolder httpConfig))
  = do
    exSet <- prepare outputFolder exclusion

    total <- getTotal options
    let num = if limit == 0 || limit > total then total else limit

    pbar <- newProgressBar $ progressBar num

    tq   <- getPosts httpConfig options exSet num
    tg   <- ThreadGroup.new
    term <- newEmptyMVar

    forkIO $ loop tq tg term pbar
    return term
 where
  loop tq tg term pbar = do
    p <- atomically $ readTMQueue tq
    case p of
      Just post -> do
        limitThreads maxWorker tg
          >> ThreadGroup.forkIO tg (downloadPost config pbar post)
        loop tq tg term pbar
      Nothing -> do
        ThreadGroup.wait tg
        complete pbar
        putMVar term True
  limitThreads limit tg = atomically (ThreadGroup.nrOfRunning tg)
    >>= \num -> when (num >= limit) $ ThreadGroup.waitN (limit `div` 2) tg

prepare :: FilePath -> FilePath -> IO ExclusionSet
prepare output exclusion = do
  createDirectoryIfMissing True output
  existing <- map takeBaseName <$> listDirectory output
  inFile   <- readEsFile exclusion
  atomically $ do
    es1 <- mkExclusionSet existing
    es2 <- mkExclusionSet inFile
    esUnion es1 es2
 where
  readEsFile esFile = case exclusion of
    "" -> return []
    _  -> lines <$> readFile exclusion

getTotal :: Option Https -> IO Int
getTotal options = runReq (getHttpConfig 0 0)
  $ reqTotal (https "konachan.com" /: "post.xml") options

getPosts
  :: HttpConfig -> Option Https -> ExclusionSet -> Int -> IO (TMQueue Post)
getPosts httpConfig option exSet limit = do
  tq <- atomically newTMQueue
  tg <- ThreadGroup.new
  forkIO $ loop 1 0 tq
  return tq
 where
  loop :: Int -> Int -> TMQueue Post -> IO ()
  loop page already tq = if limit == 0 || already < limit
    then
      runReq
          httpConfig
          (reqPosts (https "konachan.com" /: "post.json")
                    (option <> queryParam "page" (pure page))
          )
        >>= \case
              [] -> atomically $ closeTMQueue tq
              ps -> do
                newAlready <- atomically $ do
                  filtered <- takePosts already ps
                  mapM_ (writeTMQueue tq) filtered
                  return (length filtered + already)
                loop (page + 1) newAlready tq
    else atomically $ closeTMQueue tq

  takePosts already ps = filterMember exSet ps >>= \ps ->
    return $ case limit of
      0 -> ps
      _ -> take (limit - already) ps

downloadPost :: PostConfig -> ProgressBar -> Post -> IO ()
downloadPost (PostConfig kind output config) pbar p =
  runReq config (reqPost kind output p) >>= saveImage >> tick pbar

saveImage :: DownloadResult -> IO ()
saveImage (filepath, rawData) = withBinaryFile filepath WriteMode $ \h -> do
  hSetBuffering h (BlockBuffering Nothing)
  hPutStr h rawData
