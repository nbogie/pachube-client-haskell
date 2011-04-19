module PachubeClient where

import Data.List (intercalate, find)
import Data.Maybe
import Network.HTTP
import Network.URI
import Text.Regex.Posix

import Types
import XmlParse hiding (main)
import Output


data FeedStatus = StatusLive | StatusFrozen deriving (Eq, Ord, Show)
data SearchContentOption = SCSummary | SCFull deriving (Eq, Ord, Show)
data SearchOrderOption = SOCreatedAt | SORetrievedAt | SORelevance 
       deriving (Eq, Ord, Show)

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
myAPIKeyHeader ak = Header (HdrCustom "X-PachubeApiKey") (apiKey ak)

acceptHeader :: Header
acceptHeader = Header HdrAccept mimeTypeXML

contentLengthHeader :: String -> Header
contentLengthHeader body = Header (HdrCustom "Content-Length") $ show $ length body

mimeTypeXML :: String
mimeTypeXML = "application/xml"

downloadEnvironments :: ApiKey -> [SearchFilter] -> IO (Either String String)
downloadEnvironments ak fs = downloadURL ak $ makeURLGetEnvironments fs

downloadEnvironment :: ApiKey -> EnvironmentId -> IO (Either String String)
downloadEnvironment ak feedId = downloadURL ak $ makeURLGetEnvironment feedId

getEnvironments :: ApiKey -> [SearchFilter] -> IO [Environment]
getEnvironments ak fs = do
    respEither <- downloadEnvironments ak fs
    case respEither of
      Left errMsg -> do 
                      putStrLn $ "Failed" ++ errMsg
                      return []
      Right xml -> case parseEnvironmentsXML xml of
                     Just envs -> return envs
                     Nothing   -> return []

getUserEnvironments :: ApiKey -> UserName -> IO [Environment]
getUserEnvironments ak u = getEnvironments ak [UserFilter u]

getEnvironment :: ApiKey -> EnvironmentId -> IO (Either String Environment)
getEnvironment ak fId = do 
    resp <- downloadEnvironment ak fId
    -- putStrLn $ "Response: " ++ show resp
    case resp of
       Left errMsg -> return $ Left $ "failed to get feed " ++ errMsg
       Right envStr -> case parseEnvironmentXML envStr of
                          Just env     -> return $ Right env
                          Nothing      -> return $ Left "couldn't parse env"

createEnvironment :: ApiKey -> Environment -> IO (Either String EnvironmentId)
createEnvironment ak env = do
  let envText = outputEnvironment env
  respMaybe <- doPost ak makeURLCreateEnvironment envText
  case respMaybe of
    Left err -> return $ Left err
    Right resp -> do
      let hs = rspHeaders resp
      return $ case find (\h -> hdrName h == HdrLocation) hs of
                  Just (Header _ loc) -> case extractEnvironmentIdFromLocationHeader loc of
                     Nothing -> Left "Couldn't extract feed id from location header."
                     Just feedId -> Right feedId
                  Nothing -> Left "No location header."

deleteEnvironment :: ApiKey -> EnvironmentId -> IO (Either String Bool)
deleteEnvironment ak feedId = do
  rMaybe <- doDelete ak $ makeURLDeleteEnvironment feedId
  case rMaybe of
    Left err -> return $ Left err
    Right resp -> return $ Right True
    
updateEnvironment :: ApiKey -> Environment -> IO (Either String Bool)
updateEnvironment ak env = do
  let envText = outputEnvironment env
  let eId = envId env
  resp <- doPut ak (baseURL++"/feeds/"++ show eId ++".xml") envText
  print resp
  return $ Right True

updateDatastreamSimply :: ApiKey -> EnvironmentId -> DatastreamId -> String 
                          -> IO (Either String Bool)
updateDatastreamSimply ak ei di val = do
  let ds = (makeEmptyDatastream di) { dsCurrentValue = Just val }
  updateDatastream ak ei ds

updateDatastream :: ApiKey -> EnvironmentId -> Datastream -> IO (Either String Bool)
updateDatastream ak ei ds = do
  let e = makeEmptyEnvironment { envId = ei, envDatastreams = [ds]}
  let xml = outputEnvironment e
  let url = makeURLUpdateDatastream ei (dsId ds)
  respEither <- doPut ak url xml
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

{- | Download a URL.  (Left errorMessage) if an error,
(Right doc) if success. -}
downloadURL :: ApiKey -> String -> IO (Either String String)
downloadURL ak url = do 
       resp <- simpleHTTP request
       case resp of
         Left x -> return $ Left ("Error connecting: " ++ show x)
         Right r -> 
             case rspCode r of
               (2,_,_) -> return $ Right (rspBody r)
               (4,_,_) -> return $ Left (show r ++ rspBody r)
               (3,_,_) -> -- A HTTP redirect - follow if possible
                 case findHeader HdrLocation r of
                   Nothing -> return $ Left (show r)
                   Just newURL -> downloadURL ak newURL
               _ -> return $ Left (show r)
    where request = Request {rqURI = uri,
                             rqMethod = GET,
                             rqHeaders = [myAPIKeyHeader ak, acceptHeader],
                             rqBody = ""}
          uri = fromMaybe (error $ "bad url: " ++ url) $ parseURI url

doPut :: ApiKey -> String -> String -> IO (Either String (Response String))
doPut ak url body = doReq ak url PUT [myAPIKeyHeader ak, contentLengthHeader body] body

doPost :: ApiKey -> String -> String -> IO (Either String (Response String))
doPost ak url body = doReq ak url POST [myAPIKeyHeader ak, contentLengthHeader body] body

doDelete :: ApiKey -> String -> IO (Either String (Response String))
doDelete ak url = doReq ak url DELETE [myAPIKeyHeader ak] ""

doReq :: ApiKey -> String -> RequestMethod -> [Header] -> String -> IO (Either String (Response String))
doReq ak url method headers body =
    do resp <- simpleHTTP request
       case resp of
         Left x -> return $ Left ("Error connecting: " ++ show x)
         Right r -> 
             case rspCode r of
               (2,_,_) -> return $ Right r
               (4,_,_) -> return $ Left (show r ++ rspBody r)
               (3,_,_) -> -- A HTTP redirect - follow if possible
                 case findHeader HdrLocation r of
                   Nothing -> return $ Left (show r)
                   Just newURL -> doReq ak newURL method headers body
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
