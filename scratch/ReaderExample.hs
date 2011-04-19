import Control.Monad.Reader

newtype ApiKey = ApiKey { akVal :: String }
data Config = Config {apiKey :: ApiKey }
type PM = Reader Config

-- Returns True if the "count" variable contains correct bindings size.
isCountCorrect :: Config -> Bool
isCountCorrect config = runReader calc_isCountCorrect config

-- The Reader monad, which implements this complicated check.
calc_isCountCorrect :: PM Bool
calc_isCountCorrect = do
    config <- ask
    let k = apiKey config
    return (5 < length (akVal k))


runAll :: Config -> Bool
runAll c = runReader (isLongerThanEnv "foo") c

isLongerThanEnv :: String -> PM Bool
isLongerThanEnv s = do
  config <- ask
  let k = apiKey config
  return $ length (akVal k) < length s

main = do
    let c = Config { apiKey = ApiKey "myfakeapikey" }
    putStrLn $ show $ isCountCorrect c
    print $ runAll c
