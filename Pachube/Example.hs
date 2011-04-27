module Pachube.Main where

import Pachube.Types
import Pachube.PachubeClient
import Pachube.Output
import Control.Monad.Reader

-- convenience alias.
io = liftIO


demoGetEnv :: PBMonad ()
demoGetEnv = do
  -- retrieve environment by id
  env <- getEnvironment 504
  io $ print env

demoGetEnvs = do
  -- retrieve environments by tag and owner
  envs <- getEnvironments [TagFilter "air quality", UserFilter "andre"]
  io $ either error (print . map envCreator) envs

demoGetEnvs2 = do
  result <- getEnvironments [UserFilter "pachube", ContentFilter SCSummary, 
                                 PageFilter 1, PerPageFilter 5, 
                                 StatusFilter StatusLive]
  case result of
    Left err -> io $ error err 
    Right envs -> io $ print $ map envId envs

demoGetEnvs3 = do
  result <- getEnvironments [FreeTextFilter "london", PerPageFilter 1]
  -- here's another way to handle the Left and Right of the Either type
  io $ either error (print . map envId) result

demoPrintXML = putStrLn $ outputEnvironment makeTestEnvironment

demoGetEnvAndPrintXML = do
  env <- getEnvironment 504
  case env of
    Left err -> io $ putStrLn err
    Right e -> io $ putStrLn $ outputEnvironment e

demoUpdateDatastream = do
  res <- updateDatastreamSimply 23998 "0" "updatedval"
  io $ putStrLn $ show res

demoCreateEnvAndUpdate = do
  let env = makeTestEnvironment 
  envResult <- createEnvironment env
  case envResult of
    Left errMsg -> io $ putStrLn errMsg
    Right eId -> do
      upSucc <- updateDatastreamSimply eId "0" "updatedval"
      getResult <- getEnvironment eId
      case getResult of
        Left errMsg2 -> io $ putStrLn errMsg2
        Right gotEnv -> io $ putStrLn $ show gotEnv

demos = [demoGetEnv, demoGetEnvs, demoGetEnvs2, demoGetEnvs3, 
         demoGetEnvAndPrintXML, demoUpdateDatastream, 
         demoCreateEnvAndUpdate]

-- run all demos
-- main = mapM_ withApiKeyFromFile demos

-- run one demo.
main = withApiKeyFromFile demoGetEnvs

