module Main where

import Data.Maybe (fromMaybe, fromJust)
import Types
import PachubeClient
import Output

demoGet = do
  -- retrieve environments by tag and owner
  envs <- getEnvironments [TagFilter "air quality", UserFilter "andre"]
  print $ map envCreator envs
  -- retrieve environment by id
  env <- getEnvironment 504
  print env

demoGetComplex = do
  envs <- getEnvironments [UserFilter "pachube", ContentFilter SCSummary, 
               PageFilter 1, PerPageFilter 5, StatusFilter StatusLive]
  print $ map envId envs
  envsFree <- getEnvironments [FreeTextFilter "london", PerPageFilter 1]
  print $ map envId envsFree

demoPrintXML = putStrLn $ outputEnvironment makeTestEnvironment

demoGetEnvAndPrintXML = do
  env <- getEnvironment 504
  case env of
    Left err -> putStrLn err
    Right e -> putStrLn $ outputEnvironment e

demoUpdateEnv = do
  res <- updateDatastreamSimply 20319 "0" "updatedval"
  putStrLn $ show res

demoCreateEnv = do
  let env = makeTestEnvironment 
  envResult <- createEnvironment env
  case envResult of
    Left errMsg -> putStrLn errMsg
    Right eId -> do
      upSucc <- updateDatastreamSimply eId "0" "updatedval"
      getResult <- getEnvironment eId
      case getResult of
        Left errMsg2 -> putStrLn errMsg2
        Right gotEnv -> putStrLn $ show gotEnv

main = demoGetEnvAndPrintXML
