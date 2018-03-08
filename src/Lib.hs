{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Sequence
import Data.Monoid
import Data.List.Split (splitOn)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BS

import Data.Aeson
import Data.Aeson.Types
import Network.HTTP.Req
import System.FilePath

data Post = Post
  { md5 :: String
  , fileUrl :: String
  } deriving (Show)
type Posts = [Post]

instance FromJSON Post where
  parseJSON = withObject "post" $ \o -> Post <$> o .: "md5" <*> o .: "file_url"

mkParams :: (QueryParam param, Monoid param) => [(String, String)] -> param
mkParams params = foldr step (queryFlag "a") params
  where
    step (k, v) q = (T.pack k =: v) <> q

getPosts :: Url a -> Option a -> Req (Posts)
getPosts url option = do
  r <- req GET url NoReqBody lbsResponse option
  case eitherDecode $ responseBody r :: Either String Posts of
    Left e -> fail e
    Right result -> return result

getImage :: String -> Req (BS.ByteString)
getImage imageUrl =
  case (parseUrlHttps $ B.pack imageUrl) of
    Nothing -> fail "Parse URL Error"
    Just (url, option) -> do
      r <- req GET url NoReqBody lbsResponse option
      return $ responseBody r

getImagePath :: FilePath -> Post -> FilePath
getImagePath base post =
  base </> (md5 post) ++ '.' : (last $ splitOn "." (fileUrl post))
