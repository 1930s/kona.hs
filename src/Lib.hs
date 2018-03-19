{-# LANGUAGE OverloadedStrings, DataKinds, QuasiQuotes #-}

module Lib
  ( Post
  , DownloadResult
  , PostConfig(..)
  , CrawlerConfig(..)
  , reqPost
  , reqPosts
  , reqTotal
  , httpConfig
  , opts
  , parseOpts
  , progressBar
  , filterMember
  ) where

import Utils ((==:))
import qualified Utils as U

import Paths_kona (version)

import Control.Retry
import Control.Concurrent.STM

import Data.Monoid
import Data.Default
import Data.List.Split (splitOn)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashSet as HS
import Data.Aeson hiding (Parser)
import Data.Aeson.Types hiding (Parser)
import Data.Version (showVersion)

import Text.Regex.PCRE.Heavy

import System.FilePath
import System.Console.AsciiProgress

import Network.HTTP.Req hiding (header)
import qualified Network.HTTP.Client as L
import qualified Network.HTTP.Types as Y

import Options.Applicative

type BinaryContent = BS.ByteString

data Post = Post
  { md5 :: String
  , fileUrl :: String
  , previewUrl :: String
  , sampleUrl :: String
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
getUrl "sample" p = sampleUrl p
getUrl "origin" p = fileUrl p

type DownloadResult = (FilePath, BinaryContent)

reqTotal :: Url a -> Option a -> Req Int
reqTotal url option =
  req GET url NoReqBody lbsResponse (option <> "limit" ==: "1") >>=
  parse . responseBody
  where
    parse :: LB.ByteString -> Req Int
    parse lb =
      case result lb of
        Just (total, _) -> return total
        Nothing -> fail "Unable to retrieve total posts!"
    result = LB.readInt . head . snd . head . scan [re|count="(\d+)"|]

reqPosts :: Url a -> Option a -> Req [Post]
reqPosts url option = do
  r <- req GET url NoReqBody lbsResponse (option <> "limit" ==: "10")
  case (eitherDecode $ responseBody r) :: Either String [Post] of
    Left e -> fail e
    Right result -> return result

reqPost :: String -> FilePath -> Post -> Req DownloadResult
reqPost kind base p = do
  let path = getImagePath base kind p
  rawData <- reqImage (getUrl kind p)
  return (path, rawData)

reqImage :: String -> Req (BinaryContent)
reqImage imageUrl =
  case (parseUrlHttps $ B.pack imageUrl) of
    Nothing -> fail "Parse URL Error"
    Just (url, option) -> do
      r <- req GET url NoReqBody lbsResponse option
      return $ responseBody r

getImagePath :: FilePath -> String -> Post -> FilePath
getImagePath base kind post =
  base </> (md5 post) ++ '.' : (last $ splitOn "." (getUrl kind post))

httpConfig :: Int -> Int -> HttpConfig
httpConfig delay retries =
  def
  { httpConfigRetryPolicy = exponentialBackoff (delay * 1000) <> -- Convert to microseconds
                            limitRetries retries
  , httpConfigRetryJudge = judge
  }
  where
    judge _ response = statusCode response `elem` [408, 504, 524, 598, 599, 503]
    statusCode = Y.statusCode . L.responseStatus

data Opt =
  Opt [String] -- Tags
      String -- Kind
      String -- Rating
      Int -- Max worker
      FilePath -- Output
      Int -- Delay
      Int -- Retries

opt :: Parser Opt
opt =
  Opt <$> some (argument str (metavar "TAGS...")) <*>
  option
    (eitherReader $ checkWithin ratings)
    (long "rating" <> short 'r' <>
     help
       "Hentainess of the images. \
       \Choose from: safe, questionable, explict, \
       \questionableminus, questionableplus" <>
     showDefault <>
     value "safe" <>
     completeWith ratings <>
     metavar "RATING") <*>
  option
    (eitherReader $ checkWithin kinds)
    (long "kind" <> short 'k' <>
     help "Kind of image to download. \
          \Choose from: preview, sample, origin" <>
     showDefault <>
     value "origin" <>
     completeWith kinds <>
     metavar "KIND") <*>
  option
    auto
    (long "max_worker" <> short 'w' <>
     help "Limit numbers of task in parallel" <>
     showDefault <>
     value 16 <>
     metavar "MAX_WORKER") <*>
  strOption
    (long "output" <> short 'o' <> help "Output path" <> showDefault <>
     value "images" <>
     metavar "OUTPUT") <*>
  option
    auto
    (long "delay" <> short 'd' <> help "Start next try after DELAY ms" <>
     showDefault <>
     value 100 <>
     metavar "DELAY") <*>
  option
    auto
    (long "retries" <>
     help "Retry RETRIES times, otherwise the program will fail" <>
     showDefault <>
     value 5 <>
     metavar "RETRIES")
  where
    ratings =
      [ "safe"
      , "questionable"
      , "explicit"
      , "questionableminus"
      , "questionableplus"
      ]
    kinds = ["preview", "sample", "origin"]
    checkWithin sets v
      | v `elem` sets = Right v
      | otherwise = Left $ "Unknown parameter: " ++ v

opts = info (opt <**> helper)
  ( fullDesc
  <> progDesc "Download all images about TAGS"
  <> header ("kona " ++ showVersion version ++ " – A Crawler for Konachan.com"))

data CrawlerConfig =
  CrawlerConfig (Option Https) -- Query
                Int -- Max workers
                PostConfig

data PostConfig =
  PostConfig String -- Kind
             FilePath -- Output path
             HttpConfig -- HttpConfig

parseOpts (Opt t r k w o d retries) =
  CrawlerConfig
    (U.mkParams [U.tags (U.rating r : t)])
    w
    (PostConfig k o (httpConfig d retries))

progressBar :: Int -> System.Console.AsciiProgress.Options
progressBar total =
  def
  { pgFormat = "Finished :percent :bar :current/:total " ++
               "(elapsed :elapseds, :etas remaining)"
  , pgCompletedChar = '█'
  , pgPendingChar = '▁'
  , pgTotal = fromIntegral total
  , pgOnCompletion = Just "Done :percent after :elapsed seconds"
  }

filterMember :: U.ExclusionSet -> [Post] -> STM [Post]
filterMember exSet ps =
  readTVar exSet >>= \set -> return $ filter (\p -> HS.notMember (md5 p) set) ps
