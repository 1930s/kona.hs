{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Sequence
import Data.List.Split (splitOn)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BS

import Data.Aeson
import Data.Aeson.Types
import Network.HTTP.Req
import System.FilePath

type BinaryContent = BS.ByteString

data Post = Post
  { md5 :: String
  , fileUrl :: String
  , previewUrl :: String
  } deriving (Show)

instance FromJSON Post where
  parseJSON =
    withObject "post" $ \o ->
      Post <$> o .: "md5" <*> o .: "file_url" <*> o .: "preview_url"

getUrl :: Post -> String
getUrl p = fileUrl p

type DownloadResult = (FilePath, BinaryContent)

reqPosts :: Url a -> Option a -> Req [Post]
reqPosts url option = do
  r <- req GET url NoReqBody lbsResponse option
  case (eitherDecode $ responseBody r) :: Either String [Post] of
    Left e -> fail e
    Right result -> return result

reqPost :: FilePath -> Post -> Req DownloadResult
reqPost base p = do
  let path = getImagePath base p
  rawData <- reqImage (getUrl p)
  return (path, rawData)

reqImage :: String -> Req (BinaryContent)
reqImage imageUrl =
  case (parseUrlHttps $ B.pack imageUrl) of
    Nothing -> fail "Parse URL Error"
    Just (url, option) -> do
      r <- req GET url NoReqBody lbsResponse option
      return $ responseBody r

getImagePath :: FilePath -> Post -> FilePath
getImagePath base post =
  base </> (md5 post) ++ '.' : (last $ splitOn "." (getUrl post))

