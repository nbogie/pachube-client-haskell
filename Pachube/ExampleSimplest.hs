import Pachube.Types
import Pachube.PachubeClient
import Control.Monad.Reader

main = withApiKeyFromFile $ do
      res <- getEnvironments [TagFilter "air quality", UserFilter "andre"]
      case res of
        Left errMsg -> error errMsg
        Right envs -> liftIO $ print $ map envTitle envs

