module Utils where

import Control.Concurrent.STM

import Data.Monoid
import qualified Data.Text as T
import Data.HashSet

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

type ExclusionSet = TVar (Set String)

mkExclusionSet :: [String] -> STM ExclusionSet
mkExclusionSet = newTVar . foldr insert empty

exclude :: String -> ExclusionSet -> STM ()
exclude md5 exSet = modifyTVar exSet (insert md5)

