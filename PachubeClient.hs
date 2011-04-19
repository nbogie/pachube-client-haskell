module PachubeClient where

import Data.List (intercalate, find)
import Data.Maybe
import Network.HTTP
import Network.URI
import Text.Regex.Posix
import Control.Monad.Reader
import Data.Char (isSpace)

import Types
import XmlParse hiding (main)
import Output

type ErrorMsg = String

data FeedStatus = StatusLive | StatusFrozen deriving (Eq, Ord, Show)
data SearchContentOption = SCSummary | SCFull deriving (Eq, Ord, Show)
data SearchOrderOption = SOCreatedAt | SORetrievedAt | SORelevance 
       deriving (Eq, Ord, Show)

type PBMonad = ReaderT Config IO

withApiKeyFromFile :: PBMonad a -> IO a
withApiKeyFromFile fn = do
  key <- readApiKeyFromFile
  let config = Config { apiKey = key }
  runReaderT fn config

readApiKeyFromFile :: IO ApiKey 
readApiKeyFromFile = do 
  c <- readFile "api_key.txt"
  let stripped = takeWhile (not . isSpace) c
  return $ ApiKey stripped

makeConfigWithApiKeyString k = Config { apiKey = ApiKey k }

-- in getEnvironments one can supply a list of search filters
-- The rules of combination are not enforced here yet.
-- TODO: some of these are "filters", some change output format, paging.
data SearchFilter =   
                                UserFilter      String
                    |           TagFilter       String
                    |           FreeTextFilter  String
                    |           UnitFilter      String
                    |           PageFilter      Int
                    |           PerPageFilter   Int
                    |           ContentFilter   SearchContentOption
                    |           OrderFilter     SearchOrderOption
                    |           StatusFilter    FeedStatus
       deriving (Eq, Show)

-- anything that can turn itself into a URL request parameter
class Paramable a where
  toParam :: a -> String

instance Paramable SearchFilter where
  toParam (UserFilter     n)             = "user="              ++ urlEncode n
  toParam (TagFilter      n)             = "tag="               ++ urlEncode n
  toParam (FreeTextFilter n)             = "q="                 ++ urlEncode n
  toParam (UnitFilter     n)             = "unit="              ++ urlEncode n
  toParam (StatusFilter   StatusLive)    = "status=live"
  toParam (StatusFilter   StatusFrozen)  = "status=frozen"
  toParam (ContentFilter  SCSummary)     = "content=summary"
  toParam (ContentFilter  SCFull)        = "content=full"
  toParam (PageFilter     i)             = "page="              ++ show      i
  toParam (PerPageFilter  i)             = "per_page="          ++ show      i
  toParam (OrderFilter    SORelevance)   = "order=relevance"
  toParam (OrderFilter    SOCreatedAt)   = "order=created_at"
  toParam (OrderFilter    SORetrievedAt) = "order=retrieved_at"

myAPIKeyHeader :: ApiKey -> Header
myAPIKeyHeader ak = Header (HdrCustom "X-PachubeApiKey") (apiKeyVal ak)

acceptHeader :: Header
acceptHeader = Header HdrAccept mimeTypeXML

contentLengthHeader :: String -> Header
contentLengthHeader body = Header (HdrCustom "Content-Length") $ show $ length body

mimeTypeXML :: String
mimeTypeXML = "application/xml"

downloadEnvironments :: [SearchFilter] -> PBMonad (Either ErrorMsg String)
downloadEnvironments fs = downloadURL $ makeURLGetEnvironments fs

downloadEnvironment :: EnvironmentId -> PBMonad (Either ErrorMsg String)
downloadEnvironment feedId = downloadURL $ makeURLGetEnvironment feedId

getEnvironments :: [SearchFilter] -> PBMonad (Either ErrorMsg [Environment])
getEnvironments fs = do
    respEither <- downloadEnvironments fs
    case respEither of
      Left errMsg -> return $ Left errMsg
      Right xml -> case parseEnvironmentsXML xml of
                     Just envs -> return $ Right envs
                     Nothing   -> return $ Left "Failed to parse environments"

getUserEnvironments :: UserName -> PBMonad (Either ErrorMsg [Environment])
getUserEnvironments u = getEnvironments [UserFilter u]

getEnvironment :: EnvironmentId -> PBMonad (Either ErrorMsg Environment)
getEnvironment fId = do 
    resp <- downloadEnvironment fId
    -- putStrLn $ "Response: " ++ show resp
    case resp of
       Left errMsg -> return $ Left $ "failed to get feed " ++ errMsg
       Right envStr -> case parseEnvironmentXML envStr of
                          Just env     -> return $ Right env
                          Nothing      -> return $ Left "couldn't parse env"

createEnvironment :: Environment -> PBMonad (Either ErrorMsg EnvironmentId)
createEnvironment env = do
  let envText = outputEnvironment env
  respMaybe <- doPost makeURLCreateEnvironment envText
  case respMaybe of
    Left err -> return $ Left err
    Right resp -> do
      let hs = rspHeaders resp
      return $ case find (\h -> hdrName h == HdrLocation) hs of
                  Just (Header _ loc) -> case extractEnvironmentIdFromLocationHeader loc of
                     Nothing -> Left "Couldn't extract feed id from location header."
                     Just feedId -> Right feedId
                  Nothing -> Left "No location header."

deleteEnvironment :: EnvironmentId -> PBMonad (Either ErrorMsg Bool)
deleteEnvironment feedId = do
  rMaybe <- doDelete $ makeURLDeleteEnvironment feedId
  case rMaybe of
    Left err -> return $ Left err
    Right resp -> return $ Right True
    
updateEnvironment :: Environment -> PBMonad (Either ErrorMsg Bool)
updateEnvironment env = do
  let envText = outputEnvironment env
  let eId = envId env
  resp <- doPut (baseURL++"/feeds/"++ show eId ++".xml") envText
  liftIO $ print resp
  return $ Right True

updateDatastreamSimply :: EnvironmentId -> DatastreamId -> String 
                          -> PBMonad (Either ErrorMsg Bool)
updateDatastreamSimply ei di val = do
  let ds = (makeEmptyDatastream di) { dsCurrentValue = Just val }
  updateDatastream ei ds

updateDatastream :: EnvironmentId -> Datastream -> PBMonad (Either ErrorMsg Bool)
updateDatastream ei ds = do
  let e = makeEmptyEnvironment { envId = ei, envDatastreams = [ds]}
  let xml = outputEnvironment e
  let url = makeURLUpdateDatastream ei (dsId ds)
  respEither <- doPut url xml
  case respEither of
    Left err -> return $ Left err
    Right resp -> return $ Right True

baseURL :: String
baseURL = "http://api.pachube.com/v2"

makeURLCreateEnvironment = baseURL ++ "/feeds.xml"

makeURLDeleteEnvironment :: EnvironmentId -> String
makeURLDeleteEnvironment feedId = 
  baseURL ++ "/feeds/"++ show feedId ++".xml"

makeURLGetEnvironment feedId = baseURL ++ "/feeds/" ++ show feedId

makeURLGetEnvironments :: [SearchFilter] -> String
makeURLGetEnvironments fs = baseURL ++ "/feeds" ++ extra
  where 
    extra = if null kvs then "" else paramStr
    paramStr = '?' : intercalate "&" kvs
    kvs = map toParam fs

makeURLGetHistory :: EnvironmentId -> String
makeURLGetHistory feedId = baseURL ++ "/feeds/" ++ show feedId 
    ++ "?start=2010-08-02T14:01:46Z&end=2010-08-02T17:01:46Z&interval=0"

makeURLUpdateDatastream ei di = 
  baseURL++"/feeds/"++ show ei ++"/datastreams/"++ urlEncode di ++".xml"

getApiKey :: PBMonad ApiKey
getApiKey = do
  config <- ask
  return $ apiKey config

getApiKeyV :: PBMonad String
getApiKeyV = fmap apiKeyVal getApiKey

{- | Download a URL.  (Left errorMessage) if an error,
(Right doc) if success. -}
downloadURL :: String -> PBMonad (Either ErrorMsg String)
downloadURL url = do
       ak <- getApiKey
       resp <- liftIO $ simpleHTTP (request ak)
       case resp of
         Left x -> return $ Left ("Error connecting: " ++ show x)
         Right r -> 
             case rspCode r of
               (2,_,_) -> return $ Right (rspBody r)
               (4,_,_) -> return $ Left (show r ++ rspBody r)
               (3,_,_) -> -- A HTTP redirect - follow if possible
                 case findHeader HdrLocation r of
                   Nothing -> return $ Left (show r)
                   Just newURL -> downloadURL newURL
               _ -> return $ Left (show r)
    where request ak = Request { rqURI = uri,
                                 rqMethod = GET,
                                 rqHeaders = [myAPIKeyHeader ak, acceptHeader],
                                 rqBody = ""}
          uri = fromMaybe (error $ "bad url: " ++ url) $ parseURI url

doPut :: String -> String -> PBMonad (Either ErrorMsg (Response String))
doPut url body = do
  ak <- getApiKey
  doReq url PUT [myAPIKeyHeader ak, contentLengthHeader body] body

doPost :: String -> String -> PBMonad (Either ErrorMsg (Response String))
doPost url body = do
  ak <- getApiKey
  doReq url POST [myAPIKeyHeader ak, contentLengthHeader body] body

doDelete :: String -> PBMonad (Either ErrorMsg (Response String))
doDelete url = do
  ak <- getApiKey
  doReq url DELETE [myAPIKeyHeader ak] ""

doReq :: String -> RequestMethod -> [Header] -> String -> PBMonad (Either ErrorMsg (Response String))
doReq url method headers body =
    do resp <- liftIO $ simpleHTTP request
       case resp of
         Left x -> return $ Left ("Error connecting: " ++ show x)
         Right r -> 
             case rspCode r of
               (2,_,_) -> return $ Right r
               (4,_,_) -> return $ Left (show r ++ rspBody r)
               (3,_,_) -> -- A HTTP redirect - follow if possible
                 case findHeader HdrLocation r of
                   Nothing -> return $ Left (show r)
                   Just newURL -> doReq newURL method headers body
               _ -> return $ Left (show r)
    where request = Request {rqURI = uri,
                             rqMethod = method,
                             rqHeaders = headers,
                             rqBody = body }
          uri = fromMaybe (error ("bad url: " ++ url)) $ parseURI url

extractEnvironmentIdFromLocationHeader :: String -> Maybe EnvironmentId
extractEnvironmentIdFromLocationHeader url = 
  case url =~ "feeds/([0-9]+)" :: [[String]] of
    [[_, group]] -> Just $ read group
    _ -> Nothing
