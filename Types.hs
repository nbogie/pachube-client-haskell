module Types where
import Control.Monad.Reader
type EnvironmentId = Integer
type DatastreamId = String
type UserName = String

newtype ApiKey = ApiKey { apiKeyVal :: String }

data Config = Config {apiKey :: ApiKey }
type PM = Reader Config

type PachubeTimestamp = String -- TODO: impl timestamp

data Environment = Environment { 
  envId::EnvironmentId 
  , envCreator::Maybe String 
  , envTitle::Maybe String 
  , envFeed::Maybe String
  , envLocation::Maybe Location
  , envStatus::Maybe String
  , envDescription::Maybe String
  , envWebsite::Maybe String
  , envTags::[String]
  , envPrivate::Bool
  , envDatastreams::[Datastream]
} deriving (Show)

makeTestEnvironment = Environment { 
  envId = -1
  , envTitle = Just "Test feed by haskell client - title"
  , envCreator = Just "http://api.pachube.com/v2/users/sam"
  , envFeed = Nothing
  , envLocation = Nothing
  , envStatus = Nothing
  , envDescription = Just "Test feed by haskell client - description"
  , envWebsite = Just "http://example.com/feed_website"
  , envTags = ["test", "haskell"]
  , envPrivate = False
  , envDatastreams = [
    (makeEmptyDatastream  "0") {dsMinValue=Just "-1", dsMaxValue=Just "30", dsCurrentValue=Just "10"},
    (makeEmptyDatastream  "1") {dsMinValue=Just "1", dsMaxValue=Just "2", dsCurrentValue=Just "1.5"}
  ]
}
makeEmptyEnvironment = Environment { 
  envId = -1
  , envTitle = Nothing
  , envCreator = Nothing
  , envFeed = Nothing
  , envLocation = Nothing
  , envStatus = Nothing
  , envDescription = Nothing
  , envWebsite = Nothing
  , envTags = []
  , envPrivate = False
  , envDatastreams = []
}

data Datastream = Datastream { 
  dsId::String
  , dsTags::[String]
  , dsMinValue::Maybe String
  , dsMaxValue::Maybe String
  , dsCurrentValue::Maybe String
  , dsCurrentValueTimestamp :: Maybe String
  , dsDatapoints::[Datapoint] 
} deriving (Show)

makeEmptyDatastream di = Datastream di [] Nothing Nothing Nothing Nothing []

data Location = Location { 
  locationDomain::Maybe String 
  , locationExposure::Maybe String
  , locationDisposition::Maybe String
  , locationName::Maybe String
  , locationLatitude::Maybe String
  , locationLongitude::Maybe String
  , locationElevation::Maybe String
} deriving (Show, Eq)

makeEmptyLocation = Location Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data Datapoint = Datapoint { 
  datapointAt :: PachubeTimestamp
  , datapointValue::String 
} deriving (Show)

