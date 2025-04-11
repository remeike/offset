{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Web.Offset.Init where

import           Control.Concurrent.MVar
import           Control.Monad.State
import qualified Data.Map                as Map
import           Data.Text               (Text)
import qualified Database.Redis          as R
import qualified Data.IntSet as IntSet
import           TK

import           Web.Offset.Cache
import           Web.Offset.HTTP
import           Web.Offset.Internal
import           Web.Offset.Splices
import           Web.Offset.Types


initWordpress :: MonadIO m
              => WordpressConfig s m
              -> R.Connection
              -> StateT s m Text
              -> WPLens b s
              -> m (Wordpress b, Substitutions s m)
initWordpress wpconf redis getURI wpLens = do
  let rrunRedis = R.runRedis redis
  let logf = wpLogInt $ wpConfLogger wpconf
  let wpReq = case wpConfRequester wpconf of
                Left (u,p) -> wreqRequester logf u p
                Right r -> r
  active <- liftIO $ newMVar Map.empty
  let wpInt = WordpressInt{ wpRequest = wpRequestInt wpReq (wpConfEndpoint wpconf)
                          , wpCacheSet = wpCacheSetInt rrunRedis (wpConfCacheBehavior wpconf)
                          , wpCacheGet = wpCacheGetInt rrunRedis (wpConfCacheBehavior wpconf)
                          , startReqMutex = startReqMutexInt active
                          , stopReqMutex = stopReqMutexInt active
                          , runRedis = rrunRedis
                          }
  let wp = Wordpress{ wpExpireAggregates = wpExpireAggregatesInt rrunRedis
                    , wpExpirePost = wpExpirePostInt rrunRedis
                    , cachingGet = cachingGetInt wpInt
                    , cachingGetRetry = cachingGetRetryInt wpInt
                    , cachingGetError = cachingGetErrorInt wpInt
                    , cacheInternals = wpInt
                    , wpLogger = logf
                    }
  let extraFields = wpConfExtraFields wpconf
  return (wp, wordpressSubs wp extraFields getURI wpLens Nothing)
