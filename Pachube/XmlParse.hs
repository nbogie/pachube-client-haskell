module Pachube.XmlParse where
-- http://hackage.haskell.org/packages/archive/xml/latest/doc/html/Text-XML-Light-Proc.html#v:findChildren 
-- https://github.com/acw/eve/blob/2a6f930771d88d4847d96c4c86ad86a7897b945c/EVE/LowLevel/API.hs
-- http://koweycode.blogspot.com/2009/02/inkscape-layers.html

import Data.Maybe (fromJust)
import Text.XML.Light

import Pachube.Types

parseEnvironmentXML :: String -> Maybe Environment
--TODO:  error handling
parseEnvironmentXML str = (head . processEnvs ) `fmap` parseXMLDoc str 

parseEnvironmentsXML :: String -> Maybe [Environment]
parseEnvironmentsXML str = processEnvs `fmap` parseXMLDoc str

parseDatastream :: Element -> Datastream
parseDatastream el = let currentValueAt = (findChild (pxdName "current_value") el) >>= findAttr (unqual "at")
                     in Datastream {
                       -- TODO: error handling - don't use fromJust
                       dsId = fromJust $ findAttr (unqual "id") el 
                       , dsMinValue = findChildsText "min_value" el
                       , dsMaxValue = findChildsText "max_value" el
                       , dsTags = map strContent $ findChildren (pxdName "tag") el
                       , dsCurrentValue = findChildsText "current_value" el
                       , dsCurrentValueTimestamp = currentValueAt
                       , dsDatapoints = []
                     }

processEnvs :: Element -> [Environment]
processEnvs doc = let envs = findChildren (pxdName "environment") doc
                  in map processEnv envs

findChildsText :: String -> Element -> Maybe String
findChildsText childName el = strContent `fmap` findChild (pxdName childName) el

-- data Environment = Environment {envId::EnvironmentId, envTitle::Maybe String, envPrivate::Bool, envDatastreams::[Datastream]} deriving (Show)
processEnv :: Element -> Environment
processEnv envEl = 
    let aId = fromJust $ findAttr (unqual "id") envEl
    in Environment { envId = read aId
       , envCreator = findAttr (unqual "creator") envEl
       , envTitle = findChildsText "title" envEl
       , envFeed = findChildsText "feed" envEl
       , envStatus = findChildsText "status" envEl
       , envDescription = findChildsText "description" envEl
       , envTags = map strContent $ findChildren (pxdName "tag") envEl
       , envLocation = parseLocation `fmap` findChild (pxdName "location") envEl
       , envWebsite = findChildsText "website" envEl
       , envDatastreams = map parseDatastream $ findChildren (pxdName "data") envEl
       , envPrivate = "true" == (fromJust $ findChildsText "private" envEl) --TODO: error handling
       }

parseLocation :: Element -> Location
parseLocation el = makeEmptyLocation {
    locationDomain = findAttr (unqual "domain") el
  , locationExposure = findAttr (unqual "exposure") el
  , locationDisposition = findAttr (unqual "disposition") el
  , locationName = findChildsText "name" el
  , locationElevation = findChildsText "ele" el
  , locationLatitude = findChildsText "lat" el
  , locationLongitude = findChildsText "lon" el
}

basicName :: String -> QName
basicName n = QName n Nothing Nothing

pxdName :: String -> QName
pxdName n = QName n (Just "http://www.eeml.org/xsd/0.5.1") Nothing

main = demoRead

demoRead = do
  c <- getContents
  let el = fromJust $ parseXMLDoc c
  let env = processEnvs el
  print env

