module Main where

import Types
import PachubeClient
import Output
import Data.Char (isSpace)

readApiKeyFromFile :: IO ApiKey 
readApiKeyFromFile = do 
  c <- readFile "api_key.txt"
  let stripped = takeWhile (not . isSpace) c
  return $ ApiKey stripped

demoGet = do
  -- retrieve environments by tag and owner
  ak <- readApiKeyFromFile
  envs <- getEnvironments ak [TagFilter "air quality", UserFilter "andre"]
  print $ map envCreator envs
  -- retrieve environment by id
  env <- getEnvironment ak 504
  print env

demoGetComplex = do
  ak <- readApiKeyFromFile
  envs <- getEnvironments ak [UserFilter "pachube", ContentFilter SCSummary, 
               PageFilter 1, PerPageFilter 5, StatusFilter StatusLive]
  print $ map envId envs
  envsFree <- getEnvironments ak [FreeTextFilter "london", PerPageFilter 1]
  print $ map envId envsFree

demoPrintXML = putStrLn $ outputEnvironment makeTestEnvironment

demoGetEnvAndPrintXML = do
  ak <- readApiKeyFromFile
  env <- getEnvironment ak 504
  case env of
    Left err -> putStrLn err
    Right e -> putStrLn $ outputEnvironment e

demoUpdateEnv = do
  ak <- readApiKeyFromFile
  res <- updateDatastreamSimply ak 20319 "0" "updatedval"
  putStrLn $ show res

demoCreateEnv = do
  ak <- readApiKeyFromFile
  let env = makeTestEnvironment 
  envResult <- createEnvironment ak env
  case envResult of
    Left errMsg -> putStrLn errMsg
    Right eId -> do
      upSucc <- updateDatastreamSimply ak eId "0" "updatedval"
      getResult <- getEnvironment ak eId
      case getResult of
        Left errMsg2 -> putStrLn errMsg2
        Right gotEnv -> putStrLn $ show gotEnv

main = demoGetEnvAndPrintXML
