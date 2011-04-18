An experimental, incomplete haskell client for the Pachube API.

WARNING: This is the work of a haskell newbie and contains many examples of the
WRONG way to do things.  In particular the XML parsing and construction, 
and the use of the HTTP client, are naive and repetitive.

Coverage:
=========

CRUD operations for Environments are available, under the v2 API.  http://api.pachube.com/v2/

No API functions are exposed for handling Triggers, Users, or API-keys (e.g. no listing your api-keys)

Usage:
======

See Example.hs for various examples of usage.

Here's an example which retrieves environments by tag and owner, 
then show only their title:

    import PachubeClient

    main = do
      envs <- getEnvironments [TagFilter "air quality", UserFilter "andre"]
      print $ map envTitle envs

This should yield something like:
[Just "Legible Landscapes",Just "Sensor Experiments"]