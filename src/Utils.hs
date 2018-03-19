module Utils where

import Data.Monoid
import qualified Data.Text as T

import Network.HTTP.Req

(==:) :: (QueryParam p) => String -> String -> p
(==:) k v = T.pack k =: v

mkParams :: (QueryParam param, Monoid param) => [(String, String)] -> param
mkParams params = foldr step mempty params
  where
    step (k, v) q = k ==: v <> q

tags :: [String] -> (String, String)
tags tlist = ("tags", foldr step "" tlist)
  where step t ts = t ++ " " ++ ts

rating :: String -> String
rating r = "rating:" ++ r

without :: String -> String
without w = '-' : w

