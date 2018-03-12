{-# LANGUAGE OverloadedStrings, DataKinds #-}

module Main where

import Lib
import Utils

import Control.Monad.IO.Class
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
main = createFolder "image" >> serial 

createFolder :: FilePath -> IO ()
createFolder = createDirectoryIfMissing True

saveImage :: DownloadResult -> IO ()
saveImage (filepath, rawData) = do
  withBinaryFile filepath WriteMode $ \h -> do
    hSetBuffering h (BlockBuffering Nothing)
    hPutStr h rawData

getPosts :: Option Https -> IO [Post]
getPosts option = runReq def $
  reqPosts (https "konachan.com" /: "post.json") option

downloadPost :: Post -> IO ()
downloadPost p = (runReq def $ reqPost "image" p) >>= saveImage

serial :: IO ()
serial =
  getPosts (mkParams [tags ["touhou", rating "safe"]]) >>= mapM_ downloadPost
