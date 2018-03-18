{-# LANGUAGE OverloadedStrings, DataKinds, LambdaCase #-}

module Main where

import Lib
import Utils

import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TMQueue
import Data.Monoid
import System.IO hiding (hPutStr)
import System.Directory
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy (hPutStr)
import Data.Sequence

import Data.Default
import Network.HTTP.Req

main :: IO ()
main = createFolder "image" >> parallel

createFolder :: FilePath -> IO ()
createFolder = createDirectoryIfMissing True

saveImage :: DownloadResult -> IO ()
saveImage (filepath, rawData) = do
  withBinaryFile filepath WriteMode $ \h -> do
    hSetBuffering h (BlockBuffering Nothing)
    hPutStr h rawData

getPosts :: Option Https -> IO [Post]
getPosts option =
  runReq def $ reqPosts (https "konachan.com" /: "post.json") option

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
          atomically $ mapM_ (writeTMQueue tq) ps
          loop (page + 1) tq

downloadPost :: Post -> IO ()
downloadPost p = (runReq def $ reqPost "image" p) >>= saveImage

serial :: IO ()
serial =
  getPosts (mkParams [tags ["touhou", rating "safe"]]) >>= mapM_ downloadPost

parallel :: IO ()
parallel =
  getPostsChan (mkParams [tags ["touhou", rating "safe"]]) >>= loop >> return ()
  where
    loop tq = do
      p <- atomically $ readTMQueue tq
      case p of
        Just post -> do
          forkFinally (downloadPost post) handleErr
          loop tq
        Nothing -> return ()
    handleErr (Left e) = print e
    handleErr (Right _) = return ()
