{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import           Control.Retry

import           Data.Monoid

import           System.Console.AsciiProgress

import qualified Network.HTTP.Client           as L
import           Network.HTTP.Req
import qualified Network.HTTP.Types            as Y

data CrawlerConfig =
  CrawlerConfig (Option Https) -- Query
                Int -- Max workers
                Int -- Limit
                FilePath -- Exclusion file
                PostConfig

data PostConfig =
  PostConfig String -- Kind
             FilePath -- Output path
             HttpConfig -- HttpConfig

getHttpConfig :: Int -> Int -> HttpConfig
getHttpConfig delay retries = def
  { httpConfigRetryPolicy = exponentialBackoff (delay * 1000)
                              <> -- Convert to microseconds
                                 limitRetries retries
  , httpConfigRetryJudge  = judge
  }
 where
  judge _ response = statusCode response `elem` [408, 504, 524, 598, 599, 503]
  statusCode = Y.statusCode . L.responseStatus

progressBar :: Int -> System.Console.AsciiProgress.Options
progressBar total = def
  { pgFormat        = "Finished :percent :bar :current/:total "
                        ++ "(elapsed :elapseds, :etas remaining)"
  , pgCompletedChar = '█'
  , pgPendingChar   = '▁'
  , pgTotal         = fromIntegral total
  , pgOnCompletion  = Just "Done :percent after :elapsed seconds"
  }
