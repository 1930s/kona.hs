{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import Control.Monad.IO.Class
import Data.Monoid
import System.IO
import System.Directory
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BS
import Data.Sequence

import Data.Default
import Network.HTTP.Req

main :: IO ()
main =
  runReq def $ do
    r <-
      getPosts
        (https "konachan.com" /: "post.json")
        (mkParams [("tags", "rating:explicit")])
    rawData <- getImage (fileUrl $ head r)
    liftIO $ createFolder "image"
    liftIO $ saveImage (getImagePath "image" $ head r) rawData

createFolder :: FilePath -> IO ()
createFolder = createDirectoryIfMissing True

saveImage :: FilePath -> BS.ByteString -> IO ()
saveImage filepath rawData = do
  withBinaryFile filepath WriteMode $ \h -> do
    hSetBuffering h (BlockBuffering Nothing)
    BS.hPutStr h rawData

-- singleReq :: Req (a) -> IO ((B.ByteString, BS.ByteString))
-- singleReq r = 
