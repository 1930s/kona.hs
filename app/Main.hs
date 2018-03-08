{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

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
main = createFolder "image" >> serial >>= mapM_ saveImage

createFolder :: FilePath -> IO ()
createFolder = createDirectoryIfMissing True

saveImage :: DownloadResult -> IO ()
saveImage (filepath, rawData) = do
  withBinaryFile filepath WriteMode $ \h -> do
    hSetBuffering h (BlockBuffering Nothing)
    hPutStr h rawData

singleReq :: Req (Post) -> IO ((FilePath, BinaryContent))
singleReq r = runReq def $ r >>= downloadPost "image"

serial :: IO ([DownloadResult])
serial =
  runReq def $
  getPosts
    (https "konachan.com" /: "post.json")
    (mkParams [("tags", "rating:safe")]) >>=
  downloadPosts "image"
