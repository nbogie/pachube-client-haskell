An experimental, incomplete haskell client for the Pachube API.

WARNING: This is the work of a haskell newbie and contains many examples of the
WRONG way to do things.  In particular the XML parsing and construction, 
and the use of the HTTP client, are naive and repetitive.

Coverage:
=========

CRUD operations for Environments are available, under the v2 API.  http://api.pachube.com/v2/

No API functions are so far exposed for handling Triggers, Users, or API-keys (e.g. no listing your advanced API-keys)

Usage:
======

Put your API key in ./api_key.txt

See Example.hs for various examples of usage.

Here's an example which retrieves environments by tag and owner, 
then show only their title:

import Pachube.Types
import Pachube.PachubeClient
import Control.Monad.Reader

main = withApiKeyFromFile $ do
      res <- getEnvironments [TagFilter "air quality", UserFilter "andre"]
      case res of
        Left errMsg -> error errMsg
        Right envs -> liftIO $ print $ map envTitle envs

This should yield something like:
[Just "Legible Landscapes",Just "Sensor Experiments"]

You can also set your API key from a String:

withApiKey "yourkeygoeshere" $ getEnvironment 504

If you have installed the package with cabal, you can script with it in the GHCI console:

Prelude> :m +Pachube.PachubeClient Pachube.Types 

Prelude> Right envs <- withApiKey "yourAPIkeyhere" $ getEnvironments [TagFilter "arduino", PerPageFilter 5]

Prelude> map envTitle envs 
[Just "[5751] Archiduino Project: 10 (Environment)",
 Just "[5764] Archiduino Project: 11 (Building)",
 Just "House Sensors", Just "arduino_ethershield", 
 Just "Vaihingen Temperature"]

Prelude> withApiKeyFromFile $ updateDatastreamSimply 24005 "mydsid" "100.5"
