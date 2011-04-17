module Output where

import Text.XML.Light
import Text.XML.Light.Output
import Data.Maybe (fromMaybe, fromJust)
import Control.Monad
import Data.Maybe (catMaybes)
import Types

stdAttrs :: [Attr]
stdAttrs = map (\(k,v) -> Attr (unqual k) v) pairs
  where pairs = [
          ("xmlns", "http://www.eeml.org/xsd/0.5.1")
          ,("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance")
          ,("version","0.5.1")
          ,("xsi:schemaLocation", "http://www.eeml.org/xsd/0.5.1 http://www.eeml.org/xsd/0.5.1/0.5.1.xsd")
          ]

outputEnvironment :: Environment -> String
outputEnvironment e = ppTopElement $ environmentToElement e

environmentToElement :: Environment -> Element
environmentToElement env = Element (unqual "eeml") stdAttrs contents Nothing
  where contents = [Elem $ Element (unqual "environment") [] (catMaybes ([]++others)++ tags ++ dses) Nothing]
        others = map m [  ("title", envTitle)
                        , ("feed", envFeed)
                        , ("status", envStatus)
                        , ("description", envDescription)
                        , ("website", envWebsite)
                        ]
        -- tags = Just $ mkElem "tags" (map (makeTextElemSS "tag") . envTags) env
        tags = map (makeTextElemSS "tag") (envTags env)
        dses = map mkDatastream (envDatastreams env)
        m (k,f) = makeTextElemEnvF k f env
        datapoints = Nothing

mkDatastream :: Datastream -> Content
mkDatastream ds = Elem $ Element (unqual "data") [Attr (unqual "id") (dsId ds)] ((catMaybes [minVal, maxVal, currVal])++tags) Nothing
  where 
      maxVal = dsMaxValue ds >>= (return . (makeTextElemSS "max_value"))
      minVal = dsMinValue ds >>= (return . (makeTextElemSS "min_value"))
      currVal = dsCurrentValue ds >>= (return . (makeTextElemSS "current_value"))
      tags = []

mkElem :: String -> (Environment -> [Content]) -> Environment -> Content
mkElem tagName fn env = Elem $ Element (unqual tagName) [] (fn env) Nothing

makeTextElemSS :: String -> String -> Content
makeTextElemSS tagName v = 
  let tNode = Text (CData CDataText v Nothing)
  in Elem $ Element (unqual tagName) [] [tNode] Nothing

makeTextElemEnvF :: String -> (Environment -> Maybe String) -> Environment -> Maybe Content
makeTextElemEnvF tagName fn env = 
  case fn env of
    Just v -> let a = 3
                  tNode :: Content
                  tNode = Text (CData CDataText v Nothing)
              in Just $ Elem $ Element (unqual tagName) [] [tNode] Nothing
    Nothing -> Nothing
