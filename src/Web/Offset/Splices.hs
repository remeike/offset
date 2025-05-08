{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Web.Offset.Splices where

import           Control.Monad           (void)
import           Control.Monad.State
import           Control.Applicative     ((<|>))
import           Control.Lens            hiding (children)
import           Control.Concurrent.MVar
import           Data.Aeson              hiding (decode, encode, json, object)
import           Data.Aeson.Key          (fromText, toText)
import qualified Data.Aeson.KeyMap       as M
import qualified Data.Attoparsec.Text    as A
import           Data.Char               (toUpper)
import           Data.List               (lookup)
import qualified Data.Map as Map
import           Data.IntSet             (IntSet)
import qualified Data.IntSet             as IntSet
import           Data.Maybe              (fromJust, fromMaybe, catMaybes)
import           Data.Monoid
import           Data.Scientific         (floatingOrInteger)
import qualified Data.Set                as Set
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.Vector             as V
import           TK                hiding ( toText )
import qualified TK                      as TK

import           Web.Offset.Field
import           Web.Offset.Posts
import           Web.Offset.Queries
import           Web.Offset.Date
import           Web.Offset.Types
import           Web.Offset.Utils

wordpressSubs :: MonadIO m
              => Wordpress b
              -> [Field s m]
              -> StateT s m Text
              -> WPLens b s
              -> Maybe (MVar (Maybe IntSet))
              -> Substitutions s m
wordpressSubs wp extraFields getURI wpLens ids =
  subs [ ("wpPosts", wpPostsFill wp extraFields wpLens ids)
       , ("wpPostsAggregate", wpPostsAggregateFill wp extraFields wpLens ids)
       , ("wpPostByPermalink", wpPostByPermalinkFill extraFields getURI wpLens ids)
       , ("wpPage", wpPageFill wpLens)
       , ("wpNoPostDuplicates", wpNoPostDuplicatesFill wpLens ids)
       , ("wp", wpPrefetch wp extraFields getURI wpLens)
       , ("wpCustom", wpCustomFill wp)
       , ("wpCustomAggregate", wpCustomAggregateFill wp)
       , ("wpCustomDate", wpCustomDateFill)
       , ("stripHtml", stripHtmlFill)]

stripTags :: Text -> Text
stripTags ""         = ""
stripTags str = case (T.take 1 str) of
  "<" -> stripTags $ T.drop 1 $ T.dropWhile (/= '>') str
  _ -> (T.take 1 str) <> stripTags (T.drop 1 str)

stripHtmlFill :: MonadIO m => Fill s m
stripHtmlFill = Fill $ \attrs (path, tpl) lib -> do
  (output, _) <- runTemplate tpl path mempty lib
  return $ TextOutput $ stripTags $ TK.toHtml output

wpCustomDateFill :: MonadIO m => Fill s m
wpCustomDateFill =
  useAttrs (a "wp_format" % a "date") customDateFill
  where customDateFill mWPFormat date =
          let wpFormat = fromMaybe "%Y-%m-%d %H:%M:%S" mWPFormat in
          case parseWPDate wpFormat date of
              Just d -> fillChildrenWith $ datePartSubs d
              Nothing -> rawTextFill $ "<!-- Unable to parse date: " <> date <> " -->"

wpCustomFill :: MonadIO m => Wordpress b -> Fill s m
wpCustomFill wp =
  useAttrs (a "endpoint") (\e -> customFill wp (EndpointKey e []))

customFill :: MonadIO m => Wordpress b -> WPKey -> Fill s m
customFill Wordpress{..} key = Fill $ \attrs (path, tpl) lib ->
  do res <- liftIO $ (cachingGetRetry key :: IO (Either StatusCode WPResponse))
     case (fmap decodeWPResponseBody res :: Either StatusCode (Maybe Value)) of
       Left code -> do
         let notification = "Encountered status code " <> tshow code
                         <> " when querying \"" <> tshow key <> "\"."
         liftIO $ wpLogger notification
         return $ TextOutput $ "<!-- " <> notification <> " -->"
       Right (Just json) ->
        unFill (jsonToFill json) attrs (path, tpl) lib
       Right Nothing -> do
         let notification = "Unable to decode JSON for endpoint \"" <> tshow key
         liftIO $ wpLogger $ notification <> ": " <> tshow res
         return $ TextOutput $ "<!-- " <> notification <> "-->"

jsonToFill :: MonadIO m => Value -> Fill s m
jsonToFill (Object o) =
  Fill $ \_ (path, tpl) lib -> fmap fst $ runTemplate tpl path objectSubstitutions lib
  where objectSubstitutions =
          subs $ map (\k -> (transformName (toText k),
                             jsonToFill (fromJust (M.lookup k o))))
                     (M.keys o)
jsonToFill (Array v) =
  Fill $ \attrs (path, tpl) lib ->
           fmap RawTextOutput $ V.foldr mappend "" <$> V.mapM (\e -> fmap TK.toHtml $ unFill (jsonToFillArrayItem e) attrs (path, tpl) lib) v
jsonToFill (String s) = rawTextFill s
jsonToFill (Number n) = case floatingOrInteger n of
                          Left r -> rawTextFill $ tshow (r :: Double)
                          Right i -> rawTextFill $ tshow (i :: Integer)
jsonToFill (Bool True) = rawTextFill $ tshow True
jsonToFill (Bool False) = rawTextFill "<!-- JSON field found, but value is false. -->"
jsonToFill (Null) = rawTextFill "<!-- JSON field found, but value is null. -->"

jsonToFillArrayItem :: MonadIO m => Value -> Fill s m
jsonToFillArrayItem o@(Object _) = jsonToFill o
jsonToFillArrayItem a@(Array _) = jsonToFill a
jsonToFillArrayItem (String s) = fillChildrenWith $ subs [("wpArrayItem", rawTextFill s)]
jsonToFillArrayItem (Number n) =
  case floatingOrInteger n of
    Left r -> fillChildrenWith $ subs [("wpArrayItem", textFill $ tshow (r :: Double))]
    Right i -> fillChildrenWith $ subs [("wpArrayItem", textFill $ tshow (i :: Integer))]
jsonToFillArrayItem b@(Bool True) = jsonToFill b
jsonToFillArrayItem b@(Bool False) = jsonToFill b
jsonToFillArrayItem n@(Null) = jsonToFill n

wpCustomAggregateFill :: MonadIO m => Wordpress b -> Fill s m
wpCustomAggregateFill wp =
  useAttrs (a "endpoint") (customAggregateFill wp)

customAggregateFill :: MonadIO m => Wordpress b -> Text -> Fill s m
customAggregateFill Wordpress{..} endpoint = Fill $ \attrs (path, tpl) lib ->
  do let key = EndpointKey endpoint []
     res <- liftIO $ (cachingGetRetry key :: IO (Either StatusCode WPResponse))
     case (fmap decodeWPResponseBody res :: Either StatusCode (Maybe Value)) of
       Left code -> do
         let notification = "Encountered status code " <> tshow code
                         <> " when querying \"" <> endpoint <> "\"."
         liftIO $ wpLogger notification
         return $ TextOutput $ "<!-- " <> notification <> " -->"
       Right (Just json) ->
        unFill (fillChildrenWith $
                    subs [ ("wpCustomItem", jsonToFill json)
                         , ("wpCustomMeta", useAttrs (a "page") (wpAggregateMetaFill res)) ])
               attrs (path, tpl) lib
       Right Nothing -> do
         let notification = "Unable to decode JSON for endpoint \"" <> endpoint
         liftIO $ wpLogger $ notification <> ": " <> tshow res
         return $ TextOutput $ "<!-- " <> notification <> "-->"

wpPostsFill :: MonadIO m
            => Wordpress b
            -> [Field s m]
            -> WPLens b s
            -> Maybe (MVar (Maybe IntSet))
            -> Fill s m
wpPostsFill wp extraFields wpLens postIdSet = Fill $ \attrs tpl lib ->
  do (postsQuery, wpKey) <- mkPostsQueryAndKey wp attrs
     res <- liftIO $ cachingGetRetry wp wpKey
     case fmap decodeWPResponseBody res of
       Right (Just posts) -> do
         postsND <- postsWithoutDuplicates wpLens postsQuery posts postIdSet
         addPostIds postIdSet (map fst postsND)
         unFill (wpPostsHelper wp extraFields (map snd postsND)) mempty tpl lib
       Right Nothing -> return $ TextOutput ""
       Left code     -> fmap TextOutput $ liftIO $ logStatusCode wp code

postsWithoutDuplicates :: MonadIO m
                       => WPLens b s
                       -> WPQuery
                       -> [Object]
                       -> Maybe (MVar (Maybe IntSet))
                       -> StateT s m [(Int, Object)]
postsWithoutDuplicates wpLens postsQuery posts postIdSet = do
  wp <- use wpLens
  let postsW = extractPostIds posts
  case postIdSet of
    Just mvar -> do
      ids <- liftIO $ readMVar mvar
      return $ take (fromMaybe 20 $ qlimit postsQuery) . removeDupes ids $ postsW
    Nothing -> return $ take (fromMaybe 20 $ qlimit postsQuery) postsW
  where removeDupes :: Maybe IntSet -> [(Int, Object)] -> [(Int, Object)]
        removeDupes Nothing = id
        removeDupes (Just wpPostIdSet) =
          filter (\(wpId,_) -> IntSet.notMember wpId wpPostIdSet)

mkPostsQueryAndKey :: MonadIO m
                   => Wordpress b
                   -> Attributes
                   -> StateT s m (WPQuery, WPKey)
mkPostsQueryAndKey wp attrs = do
  let postsQuery = parseQueryNode (Map.toList attrs)
  filters <- liftIO $ mkFilters wp (qtaxes postsQuery)
  let wpKey = mkWPKey filters postsQuery
  return (postsQuery, wpKey)

logStatusCode :: Wordpress b -> Int -> IO Text
logStatusCode wp code = do
  let notification = "Encountered status code " <> tshow code
                     <> " when querying wpPosts."
  wpLogger wp notification
  return $ "<!-- " <> notification <> " -->"

wpPostsAggregateFill :: MonadIO m => Wordpress b
            -> [Field s m]
            -> WPLens b s
            -> Maybe (MVar (Maybe IntSet))
            -> Fill s m
wpPostsAggregateFill wp extraFields wpLens postIdSet = Fill $ \attrs tpl lib ->
  do (postsQuery, wpKey) <- mkPostsQueryAndKey wp attrs
     res <- liftIO $ cachingGetRetry wp wpKey
     case fmap decodeWPResponseBody res of
       Right (Just posts) -> do
          postsND' <- postsWithoutDuplicates wpLens postsQuery posts postIdSet
          addPostIds postIdSet (map fst postsND')
          unFill (fillChildrenWith $
                    subs [ ("wpPostsItem", wpPostsHelper wp extraFields (map snd postsND'))
                         , ("wpPostsMeta", wpAggregateMetaFill res (qpage postsQuery)) ])
                 mempty tpl lib
       Right Nothing -> return $ TextOutput ""
       Left code -> fmap TextOutput $ liftIO $ logStatusCode wp code

wpAggregateMetaFill :: MonadIO m => Either StatusCode WPResponse -> Maybe Int -> Fill s m
wpAggregateMetaFill (Right (WPResponse headers _)) mCurrentPage = do
  let totalPagesText = maybe "" T.decodeUtf8
                          (lookup "x-wp-totalpages" headers)
      totalItemsText = maybe "" T.decodeUtf8
                          (lookup "x-wp-total" headers)
      totalPages = fromMaybe 1 (readSafe totalPagesText) :: Int
      currentPage = fromMaybe 1 mCurrentPage
  fillChildrenWith $
    subs [ ("wpTotalPages", textFill totalPagesText )
         , ("wpTotalItems", textFill totalItemsText)
         , ("wpHasMorePages",
               if currentPage < totalPages
                then fillChildren
                else textFill "")
         , ("wpNoMorePages",
               if currentPage < totalPages
                then textFill ""
                else fillChildren)
         , ("wpHasMultiplePages",
               if totalPages > 1
                then fillChildren
                else textFill "")
         , ("wpHasSinglePage",
               if totalPages > 1
                then textFill ""
                else fillChildren)
         , ("wpHasPreviousPages",
               if currentPage > 1
                then fillChildren
                else textFill "")
         , ("wpHasNoPreviousPages",
               if currentPage > 1
                then textFill ""
                else fillChildren)]
wpPostsMetaFill _ _ = textFill ""

mkFilters :: Wordpress b -> [TaxSpecList] -> IO [Filter]
mkFilters wp specLists =
  concat <$> mapM (\(TaxSpecList tName list) -> catMaybes <$> mapM (toFilter tName) list) specLists
  where toFilter :: TaxonomyName -> TaxSpec -> IO (Maybe Filter)
        toFilter tName tSpec = do
          mTSpecId <- lookupSpecId wp tName tSpec
          case mTSpecId of
            Just tSpecId -> return $ Just (TaxFilter tName tSpecId)
            Nothing -> return Nothing

wpPostsHelper :: MonadIO m
              => Wordpress b
              -> [Field s m]
              -> [Object]
              -> Fill s m
wpPostsHelper wp extraFields postsND =
  mapSubs (postSubs wp extraFields) postsND

wpPostByPermalinkFill :: MonadIO m
                      => [Field s m]
                      -> StateT s m Text
                      -> WPLens b s
                      -> Maybe (MVar (Maybe IntSet))
                      -> Fill s m
wpPostByPermalinkFill extraFields getURI wpLens postIdSet = maybeFillChildrenWith' $
  do uri <- getURI
     let mperma = parsePermalink uri
     case mperma of
       Nothing -> do
         w@Wordpress{..} <- use wpLens
         liftIO $ wpLogger $ "unable to parse URI: " <> uri
         return Nothing
       Just (year, month, slug) ->
         do res <- wpGetPost wpLens (PostByPermalinkKey year month slug)
            case res of
              Just post -> do addPostIds postIdSet [fst (extractPostId post)]
                              wp <- use wpLens
                              return $ Just (postSubs wp extraFields post)
              _ -> return Nothing


feedSubs :: MonadIO m => [Field s m] -> WPLens b s -> Object -> Maybe (MVar (Maybe IntSet)) -> Substitutions s m
feedSubs fields lens obj postIdSet =
  subs $ [("wpPost", wpPostFromObjectFill fields lens obj postIdSet)]

wpPostFromObjectFill :: MonadIO m
                      => [Field s m]
                      -> WPLens b s
                      -> Object
                      -> Maybe (MVar (Maybe IntSet))
                      -> Fill s m
wpPostFromObjectFill extraFields wpLens postObj postIdSet = maybeFillChildrenWith' $
  do  addPostIds postIdSet [fst (extractPostId postObj)]
      wp <- use wpLens
      return $ Just (postSubs wp extraFields postObj)

wpNoPostDuplicatesFill :: MonadIO m => WPLens b s -> (Maybe (MVar (Maybe IntSet))) -> Fill s m
wpNoPostDuplicatesFill wpLens mPostIdSet= rawTextFill' $
  do case mPostIdSet of
       Just mvar -> liftIO $ modifyMVar_ mvar (\currentValue -> return (Just IntSet.empty))
       Nothing -> return ()
     return ""

wpPageFill :: MonadIO m => WPLens b s -> Fill s m
wpPageFill wpLens =
  useAttrs (a "name") pageFill
  where pageFill Nothing = rawTextFill ""
        pageFill (Just slug) = rawTextFill' $
         do res <- wpGetPost wpLens (PageKey slug)
            return $ case res of
                       Just page -> case M.lookup "content" page of
                                      Just (Object o) -> case M.lookup "rendered" o of
                                        Just (String r) -> r
                                        _ -> ""
                                      _ -> ""
                       _ -> ""

postSubs :: MonadIO m => Wordpress b -> [Field s m] -> Object -> Substitutions s m
postSubs wp extra object = subs (map (buildSplice object) (mergeFields postFields extra))
  where buildSplice o (F n) =
          (transformName n, rawTextFill $ getText (fromText n) o)
        buildSplice o (B n) =
          (transformName n, textFill $ getBool (fromText n) o)
        buildSplice o (Q n endpoint) =
          (transformName n, customFill wp (idToEndpoint endpoint $ getText (fromText n) o))
        buildSplice o (QM n endpoint) =
          (transformName n, customFill wp (idsToEndpoint endpoint (unArray' . M.lookup (fromText n) $ o)))
        buildSplice o (P n fill') =
          (transformName n, fill' $ getText (fromText n) o)
        buildSplice o (PV n fill') =
          (transformName n, fill' (M.lookup (fromText n) $ o))
        buildSplice o (PN n fill') =
          (transformName n, fill' (unObj . M.lookup (fromText n) $ o))
        buildSplice o (PM n fill') =
          (transformName n, fill' (unArray . M.lookup (fromText n) $ o))
        buildSplice o (N n fs) =
          (transformName n, fillChildrenWith $ subs
                            (map (buildSplice (unObj . M.lookup (fromText n) $ o)) fs))
        buildSplice o (C n path) =
          (transformName n, rawTextFill (getText (fromText $ last path) . traverseObject (init (fmap fromText path)) $ o))
        buildSplice o (CB n path) =
          (transformName n, rawTextFill (getBool (fromText $ last path) . traverseObject (init (fmap fromText path)) $ o))
        buildSplice o (CN n path fs) =
          (transformName n, fillChildrenWith $ subs
                            (map (buildSplice (traverseObject (fmap fromText path) o)) fs))
        buildSplice o (M n fs) =
          (transformName n,
            mapSubs (\(i, oinner) -> subs $ map (buildSplice oinner) fs
                                         <> [(transformName n <> "Index", textFill (tshow i))])
                    (zip [1..] (unArray . M.lookup (fromText n) $ o)))
        unValue (String t) = t
        unValue (Number i) = either (tshow :: Double -> Text)
                                    (tshow :: Integer -> Text) (floatingOrInteger i)
        unValue v = ""
        unObj (Just (Object o)) = o
        unObj _ = M.empty
        unArray (Just (Array v)) = map (unObj . Just) $ V.toList v
        unArray _ = []
        unArray' (Just (Array v)) = map unValue $ V.toList v
        unArray' _ = []
        traverseObject pth o = foldl (\o' x -> unObj . M.lookup x $ o') o pth
        getText n o = maybe "" unValue (M.lookup n o)
        getBool n o = case M.lookup n o of
                        Just (Bool b) -> tshow b
                        _ -> ""

-- * -- Internal -- * --

parseQueryNode :: [(Text, Text)] -> WPQuery
parseQueryNode attrs =
  WPPostsQuery  { qlimit   = readLookup "limit" attrs
                , qnum     = perpage
                , qoffset  = readLookup "offset" attrs
                , qpage    = readLookup "page" attrs
                , qorder   = readLookup "order" attrs
                , qorderby = lookup "orderby" attrs
                , qsearch  = lookup "search" attrs
                , qbefore  = readLookup "before" attrs
                , qafter   = readLookup "after" attrs
                , qstatus  = readLookup "status" attrs
                , qsticky  = readLookup "sticky" attrs
                , quser    = lookup "user" attrs
                , qtaxes   = filterTaxonomies attrs }
  where -- `toTitle` allows us to use the standard Read instance to, e.g.,
        -- translate the text "asc" to the type constructor `Asc`
        readLookup n attrs = (readSafe . T.toTitle) =<< lookup n attrs
        perpage =
          case readLookup "per-page" attrs of
            Just n -> Just n
            Nothing -> readLookup "num" attrs

listOfFilters = ["limit"
                , "num"
                , "offset"
                , "page"
                , "per-page"
                , "user"
                , "order"
                , "orderby"
                , "context"
                , "search"
                , "after"
                , "before"
                , "slug"
                , "status"
                , "sticky"]

filterTaxonomies :: [(Text, Text)] -> [TaxSpecList]
filterTaxonomies attrs =
  let taxAttrs = filter (\(k, _) -> (k `notElem` listOfFilters)) attrs in
  map attrToTaxSpecList taxAttrs

taxDictKeys :: [TaxSpecList] -> [WPKey]
taxDictKeys = map (\(TaxSpecList tName _) -> TaxDictKey tName)

wpPrefetch :: MonadIO m
           => Wordpress b
           -> [Field s m]
           -> StateT s m Text
           -> WPLens b s
           -> Fill s m
wpPrefetch wp extra uri wpLens = Fill $ \ _m (p, tpl) l -> do
    Wordpress{..} <- use wpLens
    mKeys <- liftIO $ newMVar []
    void $ runTemplate tpl p (prefetchSubs wp mKeys) l
    newPostIdSet <- liftIO $ newMVar Nothing
    wpKeys <- liftIO $ readMVar mKeys
    void $ liftIO $ concurrently $ map cachingGet wpKeys
    fmap fst $ runTemplate tpl p (wordpressSubs wp extra uri wpLens (Just newPostIdSet)) l

prefetchSubs :: MonadIO m => Wordpress b -> MVar [WPKey] -> Substitutions s m
prefetchSubs wp mkeys =
  subs [ ("wpPosts", wpPostsPrefetch wp mkeys)
       , ("wpPage", useAttrs (a"name") $ wpPagePrefetch mkeys) ]

wpPostsPrefetch :: MonadIO m
                => Wordpress b
                -> MVar [WPKey]
                -> Fill s m
wpPostsPrefetch wp mKeys = Fill $ \attrs _ _ ->
  do let postsQuery = parseQueryNode (Map.toList attrs)
     filters <- liftIO $ mkFilters wp (qtaxes postsQuery)
     let key = mkWPKey filters postsQuery
     liftIO $ modifyMVar_ mKeys (\keys -> return $ key : keys)
     return $ TextOutput ""

wpPagePrefetch :: MonadIO m
               => MVar [WPKey]
               -> Text
               -> Fill s m
wpPagePrefetch mKeys name = rawTextFill' $
  do let key = PageKey name
     liftIO $ modifyMVar_ mKeys (\keys -> return $ key : keys)
     return ""

mkWPKey :: [Filter]
    -> WPQuery
    -> WPKey
mkWPKey taxFilters wppq@WPPostsQuery{..} =
  PostsKey (Set.fromList $ toFilters wppq ++ taxFilters ++ userFilter quser)
  where userFilter Nothing = []
        userFilter (Just u) = [UserFilter u]

toFilters :: WPQuery -> [Filter]
toFilters WPPostsQuery{..} =
  catMaybes [ NumFilter <$> qnum
            , PageFilter <$> qpage
            , OffsetFilter <$> qoffset
            , OrderFilter <$> qorder
            , OrderByFilter <$> qorderby
            , SearchFilter <$> qsearch
            , BeforeFilter <$> qbefore
            , AfterFilter <$> qafter
            , StatusFilter <$> qstatus
            , StickyFilter <$> qsticky]

findDict :: [(TaxonomyName, TaxSpec -> TaxSpecId)] -> TaxSpecList -> [Filter]
findDict dicts (TaxSpecList tName tList) =
  case lookup tName dicts of
    Just dict -> map (TaxFilter tName . dict) tList
    Nothing -> []

parsePermalink :: Text -> Maybe (Text, Text, Text)
parsePermalink = either (const Nothing) Just . A.parseOnly parser . T.reverse
  where parser = do _ <- A.option ' ' (A.char '/')
                    guls <- A.many1 (A.letter <|> A.char '-' <|> A.digit)
                    _ <- A.char '/'
                    segment2 <- A.many1 (A.letter <|> A.char '-' <|> A.digit)
                    _ <- A.char '/'
                    segment1 <- A.many1 (A.letter <|> A.char '-' <|> A.digit)
                    _ <- A.char '/'
                    return (T.reverse $ T.pack segment1
                           ,T.reverse $ T.pack segment2
                           ,T.reverse $ T.pack guls)

wpGetPost :: (MonadState s m, MonadIO m) => WPLens b s -> WPKey -> m (Maybe Object)
wpGetPost wpLens wpKey =
  do wp <- use wpLens
     liftIO $ getPost wp wpKey

getPost :: Wordpress b -> WPKey -> IO (Maybe Object)
getPost Wordpress{..} wpKey = decodePost <$> cachingGetRetry wpKey
  where decodePost :: Either StatusCode WPResponse -> Maybe Object
        decodePost (Right t) =
          do post' <- decodeWPResponseBody t
             case post' of
              Just (post:_) -> Just post
              _ -> Nothing
        decodePost (Left _) = Nothing


transformName :: Text -> Text
transformName = T.append "wp" . snd . T.foldl f (True, "")
  where f (True, rest) next = (False, T.snoc rest (toUpper next))
        f (False, rest) '_' = (True, rest)
        f (False, rest) '-' = (True, rest)
        f (False, rest) next = (False, T.snoc rest next)

-- Move this into Init.hs (should retrieve from Wordpress data structure)
addPostIds :: (MonadState s m, MonadIO m) => Maybe (MVar (Maybe IntSet)) -> [Int] -> m ()
addPostIds mIdSetMVar ids =
  case mIdSetMVar of
    Just idSetMVar -> liftIO $ modifyMVar_ idSetMVar addIds
    Nothing -> return ()
  where addIds :: Maybe IntSet -> IO (Maybe IntSet)
        addIds (Just currentSet) = return (Just (currentSet `IntSet.union` (IntSet.fromList ids)))
        addIds Nothing = return Nothing

idToEndpoint :: IdToEndpoint -> Text -> WPKey
idToEndpoint (UseId endpoint) id = EndpointKey (endpoint <> id) []
idToEndpoint (UseSlug endpoint) slug = EndpointKey (endpoint) [("slug", slug)]

idsToEndpoint :: IdsToEndpoint -> [Text] -> WPKey
idsToEndpoint (UseInclude endpoint) ids = EndpointKey endpoint (map (\id -> ("include[]", id)) ids)

{-# ANN module ("HLint: ignore Eta reduce" :: String) #-}
