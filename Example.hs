module Main where

import Data.Maybe (fromMaybe, fromJust)
import Types
import PachubeClient
import Output

demoGet = do
  -- retrieve environments by tag and owner
  envs <- (getEnvironments [("tag", "air quality"), ("user", "andre")])
  print $ map envCreator envs
  -- retrieve environment by id
  env <- getEnvironment 504
  print env

demoPrintXML = do
  putStrLn $ outputEnvironment makeTestEnvironment

demoGetEnvAndPrintXML = do
  env <- getEnvironment 504
  case env of
    Left err -> putStrLn err
    Right e -> putStrLn $ outputEnvironment e

demoCreateEnv = do
  let env = makeTestEnvironment 
  fmap print $ createEnvironment env

main = demoGetEnvAndPrintXML
