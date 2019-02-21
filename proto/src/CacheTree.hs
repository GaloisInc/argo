{-# Language BangPatterns #-}
module CacheTree
  ( Cache(..)
  , newCache
  , cacheLookup
  , cacheAdvance
  ) where

import           Control.Concurrent
import           Control.Monad
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Crypto.Hash.SHA1 as SHA1
import           Data.Hashable (Hashable)

data Cache s c = Cache
  { cacheRoot :: s
  , cacheBranches :: MVar (HashMap c (MVar (Cache s c)))
  }

newCache :: s -> IO (Cache s c)
newCache s = Cache s <$> newMVar HashMap.empty

cacheLookup ::
  Eq c =>
  Hashable c =>
  (c -> s -> IO s)      {- ^ run command -} ->
  Cache s c             {- ^ cache       -} ->
  [c]                   {- ^ commands    -} ->
  IO (Cache s c)
cacheLookup runStep = foldM (cacheAdvance runStep)

cacheAdvance ::
  Eq c =>
  Hashable c =>
  (c -> s -> IO s)      {- ^ run command -} ->
  Cache s c             {- ^ cache       -} ->
  c                     {- ^ command     -} ->
  IO (Cache s c)
cacheAdvance runStep (Cache s var) step =
  do (found, nextVar) <-
       modifyMVar var $ \hashMap ->
         case HashMap.lookup step hashMap of
           Just nextVar -> return (hashMap, (True, nextVar))
           Nothing ->
             do nextVar <- newEmptyMVar
                let !hashMap' = HashMap.insert step nextVar hashMap
                return (hashMap', (False, nextVar))

     if found then
       do readMVar nextVar

     else
       do cache <- newCache =<< runStep step s
          putMVar nextVar cache
          return cache
