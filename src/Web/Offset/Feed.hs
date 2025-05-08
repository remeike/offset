{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}

module Web.Offset.Feed where

import           Control.Monad.State
import           Data.Aeson          hiding (decode, encode, json, object)
import           Data.Aeson.Types    (parseMaybe)
import           Data.Maybe          (maybeToList, fromJust)
import           Data.Monoid
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T
import           Data.Time.Clock
import           Data.Time.Format    (formatTime, defaultTimeLocale)
import           Text.XML.Light
import           Web.Atom            hiding (Link)
import qualified Web.Atom            as A (Link (..))
import           Text.RSS            hiding (Link)
import qualified Text.RSS            as R (ItemElem (..))
import           Network.URI         (parseURI, URI)

import           Web.Offset.Date
import           Web.Offset.Link     as O
import           Web.Offset.Splices
import           Web.Offset.Types
import           Web.Offset.Utils

data FeedFormat = AtomFeed | RSSFeed

data WPFeed m =
  WPFeed { wpFeedFormat  :: FeedFormat
         , wpFeedURI     :: T.Text
         , wpFeedTitle   :: T.Text
         , wpFeedIcon    :: Maybe T.Text
         , wpFeedLogo    :: Maybe T.Text
         , wpBaseURI     :: T.Text
         , wpBuildLinks  :: Object -> [O.Link]
         , wpGetAuthors  :: WPAuthorStyle
         , wpRenderEntry :: Object -> m (Maybe T.Text) }

data WPAuthorStyle = GuestAuthors | DefaultAuthor

makeItem :: MonadIO m
         => Wordpress b
         -> WPFeed m
         -> WPEntry
         -> m [ItemElem]
makeItem wp wpFeed entry@WPEntry{..} = do
  content <- wpEntryContent (wpRenderEntry wpFeed) entry
  let guid = entryGuid (wpBaseURI wpFeed) wpEntryId wpEntryJSON
  let baseEntry = makeEntry guid (TextHTML wpEntryTitle) wpEntryUpdated
  authors <- liftIO $ case wpGetAuthors wpFeed of
               GuestAuthors -> getAuthorsInline wpEntryJSON
               DefaultAuthor -> getAuthorViaReq wp wpEntryJSON
  return [
            Title (T.unpack wpEntryTitle)
          , R.Link $ fromJust $ parseURI $ T.unpack $ O.linkHref $ head $ wpBuildLinks wpFeed wpEntryJSON
          , Description (T.unpack wpEntrySummary)
          , Author $ "rss@jacobin.com" ++ " (" ++ (T.unpack $ personName (unWP (head authors))) ++ ")"
          , PubDate wpEntryPublished
          ]

generateRSSFeed :: MonadIO m => Wordpress b -> WPFeed m -> m String
generateRSSFeed wp wpFeed = do
    wpEntries <- liftIO $  getWPEntries wp
    let mostRecentUpdate = maximum (map wpEntryUpdated wpEntries)

    items <- mapM (makeItem wp wpFeed) wpEntries

    let rss = RSS  (T.unpack $ wpFeedTitle wpFeed) (fromJust (parseURI (T.unpack (wpFeedURI wpFeed)))) ""
                [ Language "en-us"
                , ChannelPubDate mostRecentUpdate
                , LastBuildDate mostRecentUpdate
                , TTL 60
                ]
                items

    return $ showXML $ rssToXML rss

toXMLFeed :: MonadIO m => Wordpress b -> WPFeed m -> m T.Text
toXMLFeed wp wpFeed@(WPFeed format uri title icon logo _ _ _ _) = do
  wpEntries <- liftIO $ getWPEntries wp
  let mostRecentUpdate = maximum (map wpEntryUpdated wpEntries)
  entries <- mapM (toEntry wp wpFeed) wpEntries
  let feed = (makeFeed (unsafeURI $ T.unpack uri) (TextPlain title) mostRecentUpdate)
             { feedIcon = unsafeURI <$> T.unpack <$> icon
             , feedLogo = unsafeURI <$> T.unpack <$> logo
             , feedEntries = entries }
  case format of
    AtomFeed ->
      return $ T.pack $ ppTopElement $ fixNamespace $ feedXML xmlgen feed
    RSSFeed -> do
      feed <- generateRSSFeed wp wpFeed
      return $ T.pack feed

fixNamespace :: Element -> Element
fixNamespace el@(Element _name attrs _content _line) =
  el { elAttribs = Attr (QName "xmlns" Nothing Nothing) "http://www.w3.org/2005/Atom" : attrs }

-- Copy-pasted from atom-basic docs
xmlgen :: XMLGen Element Text.XML.Light.Content QName Attr
xmlgen = XMLGen
    { xmlElem     = \n as ns    -> Element n as ns Nothing
    , xmlName     = \nsMay name -> QName (T.unpack name)
                                          (fmap T.unpack nsMay) Nothing
    , xmlAttr     = \k v        -> Attr k (T.unpack v)
    , xmlTextNode = \t          -> Text $ CData CDataText (T.unpack t) Nothing
    , xmlElemNode = Elem }

getWPEntries :: Wordpress b -> IO [WPEntry]
getWPEntries wp = do
  res <- liftIO $ cachingGetRetry wp (mkWPKey [] allPostsQuery)
  case res of
    Left statusCode -> error $ "Status code error: " ++ show statusCode
    Right resp ->
      case decodeWPResponseBody resp of
        Just posts -> return posts
        Nothing -> error $ "Couldn't decode: " <> show resp

allPostsQuery :: WPQuery
allPostsQuery =
  WPPostsQuery  { qlimit   = Just 20
                , qnum     = Just 20
                , qoffset  = Nothing
                , qpage    = Nothing
                , qorder   = Nothing
                , qorderby = Nothing
                , qsearch  = Nothing
                , qbefore  = Nothing
                , qafter   = Nothing
                , qstatus  = Nothing
                , qsticky  = Nothing
                , quser    = Nothing
                , qtaxes   = [] }

wpEntryContent :: MonadIO m
               => (Object -> m (Maybe T.Text))
               -> WPEntry
               -> m (Maybe (Web.Atom.Content e))
wpEntryContent renderer wpentry =
  (fmap . fmap) InlineHTMLContent (renderer $ wpEntryJSON wpentry)

toEntry :: MonadIO m
        => Wordpress b
        -> WPFeed m
        -> WPEntry
        -> m (Entry e)
toEntry wp wpFeed entry@WPEntry{..} = do
  content <- wpEntryContent (wpRenderEntry wpFeed) entry
  let guid = entryGuid (wpBaseURI wpFeed) wpEntryId wpEntryJSON
  let baseEntry = makeEntry guid (TextHTML wpEntryTitle) wpEntryUpdated
  authors <- liftIO $ case wpGetAuthors wpFeed of
               GuestAuthors -> getAuthorsInline wpEntryJSON
               DefaultAuthor -> getAuthorViaReq wp wpEntryJSON
  return $ baseEntry { entryPublished = Just wpEntryPublished
                     , entrySummary = Just (TextHTML wpEntrySummary)
                     , entryContent = content
                     , entryAuthors = map unWP authors
                     , entryLinks = map toAtomLink (wpBuildLinks wpFeed wpEntryJSON)}

toAtomLink :: Link -> A.Link
toAtomLink (Link href title) =
  A.Link { linkHref = unsafeURI $ T.unpack href
         , linkRel = Nothing
         , linkType = Nothing
         , linkHrefLang = Nothing
         , linkTitle = Just title
         , linkLength = Nothing }

data WPEntry =
  WPEntry { wpEntryId        :: Int
          , wpEntryTitle     :: T.Text
          , wpEntryUpdated   :: UTCTime
          , wpEntryPublished :: UTCTime
          , wpEntrySummary   :: T.Text
          , wpEntryJSON      :: Object } deriving (Eq, Show)

instance FromJSON WPEntry where
  parseJSON (Object v) =
    WPEntry <$> v .: "id" <*>
                (do t <- v .: "title"
                    t .: "rendered") <*>
                (jsonParseDate <$> (v .:"modified")) <*>
                (jsonParseDate <$> (v .: "date")) <*>
                (do e <- v .: "excerpt"
                    e .: "rendered") <*>
                return v
  parseJSON _ = error "bad post"

newtype WPPerson = WPPerson { unWP :: Person } deriving (Eq, Show)

instance FromJSON WPPerson where
  parseJSON (Object v) =
    WPPerson <$> (Person <$> v .: "name" <*> return Nothing <*> return Nothing)
  parseJSON _ = error "bad author"

getAuthorsInline :: Object -> IO [WPPerson]
getAuthorsInline v =
  do let authors = parseMaybe (\obj -> obj .: "authors") v
     case authors of
       Just list -> return list
       Nothing   -> return []

getAuthorViaReq :: Wordpress b -> Object -> IO [WPPerson]
getAuthorViaReq wp v =
  do let mAuthorId = parseMaybe (\obj -> obj .: "author") v :: Maybe Int
     case mAuthorId of
       Nothing -> return []
       Just authorId ->
         do eRespError <- cachingGetRetry wp (EndpointKey ("wp/v2/users/" <> tshow authorId) [])
            case eRespError of
              Left _ -> return []
              Right resp ->
                let mAuthorName = decodeWPResponseBody resp in
                  case mAuthorName of
                    Nothing -> return []
                    Just authorName ->return (maybeToList authorName)

entryGuid :: T.Text -> Int -> Object -> URI
entryGuid baseURI wpId wpJSON =
  unsafeURI $ T.unpack $
    case buildPermalink baseURI wpJSON of
      Just permalink -> O.linkHref permalink
      Nothing -> baseURI <> "/posts?id=" <> tshow wpId
