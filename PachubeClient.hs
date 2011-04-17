module PachubeClient where

import Data.List (intercalate, sort, find)
import Data.Maybe
import Network.HTTP
import Network.URI
import Text.Regex.Posix

import Types
import PachubeClientAPIKey
import XmlParse hiding (main)
import Output

myAPIKeyHeader :: Header
myAPIKeyHeader = Header (HdrCustom "X-PachubeApiKey") getAPIKey

acceptHeader :: Header
acceptHeader = Header HdrAccept mimeTypeXML

contentLengthHeader :: String -> Header
contentLengthHeader body = Header (HdrCustom "Content-Length") $ show $ length body

mimeTypeXML :: String
mimeTypeXML = "application/xml"

downloadEnvironments :: [SearchFilter] -> IO (Either String String)
downloadEnvironments fs = downloadURL $ makeURLGetEnvironments fs

downloadEnvironment :: EnvironmentId -> IO (Either String String)
downloadEnvironment feedId = downloadURL $ makeURLGetEnvironment feedId

type SearchFilter = (String,String)

getEnvironments :: [SearchFilter] -> IO [Environment]
getEnvironments fs = do
    respEither <- downloadEnvironments fs
    case respEither of
      Left errMsg -> do 
                        putStrLn $ "Failed" ++ errMsg
                        return []
      Right xml -> case parseEnvironmentsXML xml of
                     Just envs -> return envs
                     Nothing   -> return []

getEnvironment :: EnvironmentId -> IO (Either String Environment)
getEnvironment fId = do 
    resp <- downloadEnvironment fId
    -- putStrLn $ "Response: " ++ show resp
    case resp of
       Left errMsg -> return $ Left $ "failed to get feed " ++ errMsg
       Right envStr -> case parseEnvironmentXML envStr of
                          Just env     -> return $ Right env
                          Nothing      -> return $ Left "couldn't parse env"

createEnvironment :: Environment -> IO (Either String EnvironmentId)
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

deleteEnvironment :: EnvironmentId -> IO (Either String Bool)
deleteEnvironment feedId = do
  rMaybe <- doDelete $ makeURLDeleteEnvironment feedId
  case rMaybe of
    Left err -> return $ Left err
    Right resp -> return $ Right True
    
updateEnvironment :: Environment -> IO (Either String Bool)
updateEnvironment env = do
  let envText = outputEnvironment env
  let eId = envId env
  resp <- doPut (baseURL++"/feeds/"++ show eId ++".xml") envText
  print resp
  return $ Right True

baseURL :: String
baseURL = "http://api.pachube.com/v2"

makeURLCreateEnvironment = (baseURL ++ "/feeds.xml")

makeURLDeleteEnvironment :: EnvironmentId -> String
makeURLDeleteEnvironment feedId = 
  baseURL ++ "/feeds/"++ show feedId ++".xml"

makeURLGetEnvironment feedId = baseURL ++ "/feeds/" ++ show feedId

makeURLGetEnvironments :: [SearchFilter] -> String
makeURLGetEnvironments fs = baseURL ++ "/feeds" ++ extra
  where 
    extra = if null kvs then "" else paramStr
    paramStr = '?' : intercalate "&" kvs
    kvs = map (\(k,v) -> urlEncode k ++ "=" ++ urlEncode v) fs

makeURLGetHistory :: EnvironmentId -> String
makeURLGetHistory feedId = baseURL ++ "/feeds/" ++ show feedId 
                           ++ "?start=2010-08-02T14:01:46Z&end=2010-08-02T17:01:46Z&interval=0"

{- | Download a URL.  (Left errorMessage) if an error,
(Right doc) if success. -}
downloadURL :: String -> IO (Either String String)
downloadURL url =
    do resp <- simpleHTTP request
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
    where request = Request {rqURI = uri,
                             rqMethod = GET,
                             rqHeaders = [myAPIKeyHeader, acceptHeader],
                             rqBody = ""}
          uri = fromMaybe (error $ "bad url: " ++ url) $ parseURI url

doPut :: String -> String -> IO (Either String (Response String))
doPut url body = doReq url PUT [myAPIKeyHeader, contentLengthHeader body] body

doPost :: String -> String -> IO (Either String (Response String))
doPost url body = doReq url POST [myAPIKeyHeader, contentLengthHeader body] body

doDelete :: String -> IO (Either String (Response String))
doDelete url = doReq url DELETE [myAPIKeyHeader] ""

doReq :: String -> RequestMethod -> [Header] -> String -> IO (Either String (Response String))
doReq url method headers body =
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
