{-# LANGUAGE OverloadedStrings, DataKinds, QuasiQuotes #-}

module Lib
  ( Post
  , DownloadResult
  , reqPost
  , reqPosts
  , reqTotal
  , httpConfig
  , opts
  , parseOpts
  ) where

import Utils ((==:))
import qualified Utils as U

import Control.Retry

import Data.Monoid
import Data.Default
import Data.Sequence
import Data.List.Split (splitOn)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Lazy as BS
import Data.Aeson hiding (Parser)
import Data.Aeson.Types hiding (Parser)
import Text.Regex.PCRE.Heavy

import System.FilePath

import Network.HTTP.Req hiding (header)
import qualified Network.HTTP.Client as L
import qualified Network.HTTP.Types as Y

import Options.Applicative

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

reqTotal :: Url a -> Option a -> Req Int
reqTotal url option =
  req GET url NoReqBody lbsResponse (option <> "limit" ==: "1") >>= parse . responseBody
  where
    parse :: LB.ByteString -> Req Int
    parse lb =
      case result lb of
        Just (total, _) -> return total
        Nothing -> fail "Unable to retrieve total posts!"
    result = LB.readInt . head . snd . head . scan [re|count="(\d+)"|]

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

httpConfig :: HttpConfig
httpConfig =
  def
  { httpConfigRetryPolicy = exponentialBackoff 100000 -- <> limitRetries 5
  , httpConfigRetryJudge = judge
  }
  where
    judge _ response = statusCode response `elem` [408, 504, 524, 598, 599, 503]
    statusCode = Y.statusCode . L.responseStatus

data Opt = Opt
  { tags :: [String]
  , rating :: String
  , maxWorker :: Int
  , outputFolder :: String
  }

opt :: Parser Opt
opt =
  Opt <$> some (argument str (metavar "TAGS...")) <*>
  option
    (eitherReader checkRating)
    (long "rating" <> short 'r' <> help "Hentainess of the images" <>
     showDefault <>
     value "safe" <>
     completeWith ratings <>
     metavar "RATING") <*>
  option
    auto
    (long "max_worker" <> short 'w' <> help "Limit threads number" <>
     showDefault <>
     value 16 <>
     metavar "MAX_WORKER") <*>
  strOption
    (long "output" <> short 'o' <> help "Output path" <>
     showDefault <>
     value "images" <>
     metavar "OUTPUT")
  where
    ratings =
      [ "safe"
      , "questionable"
      , "explicit"
      , "questionableminus"
      , "questionableplus"
      ]
    checkRating r
      | r `elem` ratings = Right r
      | otherwise = Left $ "Unknown rating: " ++ r

opts = info (opt <**> helper)
  ( fullDesc
  <> progDesc "Download all images of TAGS"
  <> header "kona – A Crawler for Konachan.com")

parseOpts :: Opt -> (Option Https, Int, FilePath)
parseOpts (Opt t r w o) = (U.mkParams [U.tags (U.rating r : t)], w, o)
