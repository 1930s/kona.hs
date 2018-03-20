module Utils where

import Control.Concurrent.STM

import Data.Monoid
import qualified Data.Text as T
import Data.HashSet

import Network.HTTP.Req hiding ((=:))

(=:) :: (QueryParam p) => String -> String -> p
(=:) k v = queryParam (T.pack k) (pure v)

mkParams :: (QueryParam param, Monoid param) => [(String, String)] -> param
mkParams params = foldr step mempty params
  where
    step (k, v) q = k =: v <> q

tags :: [String] -> (String, String)
tags tlist = ("tags", foldr step "" tlist)
  where step t ts = t ++ " " ++ ts

rating :: String -> String
rating r = "rating:" ++ r

order :: String -> String
order o = "order:" ++ o

type ExclusionSet = TVar (Set String)

mkExclusionSet :: [String] -> STM ExclusionSet
mkExclusionSet = newTVar . fromList

exclude :: String -> ExclusionSet -> STM ()
exclude md5 exSet = modifyTVar exSet (insert md5)

esUnion :: ExclusionSet -> ExclusionSet -> STM ExclusionSet
esUnion es1 es2 = do
  set1 <- readTVar es1
  set2 <- readTVar es2
  newTVar $ union set1 set2

