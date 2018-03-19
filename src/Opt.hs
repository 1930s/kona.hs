module Opt
  ( opts
  , parseOpts
  ) where

import Paths_kona (version)
import Utils
import Config

import Data.Monoid
import Data.Version (showVersion)

import Options.Applicative

import Network.HTTP.Req hiding (header)

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

parseOpts (Opt t r k w o d retries) =
  CrawlerConfig
    (mkParams [tags (rating r : t)])
    w
    (PostConfig k o (httpConfig d retries))
