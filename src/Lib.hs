{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Lib
  ( Post
  , DownloadResult
  , reqPost
  , reqPosts
  , reqTotal
  , filterMember
  ) where

import           Utils

import           Control.Concurrent.STM

import           Data.Aeson
import qualified Data.ByteString
import qualified Data.ByteString.Char8  as B
import qualified Data.HashSet           as HashSet
import           Data.List.Split        (splitOn)
import           Data.Monoid

import           Text.Regex.PCRE.Heavy

import           System.FilePath

import           Network.HTTP.Req       hiding ((=:))

type BinaryContent = Data.ByteString.ByteString

data Post = Post
  { md5        :: String
  , fileUrl    :: String
  , previewUrl :: String
  , sampleUrl  :: String
  } deriving (Show)

instance FromJSON Post where
  parseJSON =
    withObject "post" $ \o ->
      Post <$> o .: "md5" <*>
               o .: "file_url" <*>
               o .: "preview_url" <*>
               o .: "sample_url"


getUrl :: String -> Post -> String
getUrl "preview" p = previewUrl p
getUrl "sample" p  = sampleUrl p
getUrl "origin" p  = fileUrl p
getUrl _ _         = fail "Wrong type of request!"

type DownloadResult = (FilePath, BinaryContent)

reqTotal :: Url a -> Option a -> Req Int
reqTotal url option =
  req GET url NoReqBody bsResponse (option <> "limit" =: "1") >>=
  parse . responseBody
  where
    parse :: B.ByteString -> Req Int
    parse lb =
      case result lb of
        Just (total, _) -> return total
        Nothing         -> fail "Unable to retrieve total posts!"
    result = B.readInt . head . snd . head . scan [re|count="(\d+)"|]

reqPosts :: Url a -> Option a -> Req [Post]
reqPosts url option = do
  r <- req GET url NoReqBody lbsResponse (option <> "limit" =: "10")
  case (eitherDecode $ responseBody r) :: Either String [Post] of
    Left e       -> fail e
    Right result -> return result

reqPost :: String -> FilePath -> Post -> Req DownloadResult
reqPost kind base p = do
  let path = getImagePath base kind p
  rawData <- reqImage (getUrl kind p)
  return (path, rawData)

reqImage :: String -> Req BinaryContent
reqImage imageUrl =
  case parseUrlHttps $ B.pack imageUrl of
    Nothing -> fail "Parse URL Error"
    Just (url, option) ->
      responseBody <$> req GET url NoReqBody bsResponse option

getImagePath :: FilePath -> String -> Post -> FilePath
getImagePath base kind post =
  base </> md5 post ++ '.' : last (splitOn "." (getUrl kind post))

filterMember :: ExclusionSet -> [Post] -> STM [Post]
filterMember exSet ps =
  readTVar exSet >>= \set -> return $ filter (\p -> HashSet.notMember (md5 p) set) ps
