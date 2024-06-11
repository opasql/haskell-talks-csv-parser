module Parse.Valeri where

import           Control.Applicative
import           Prelude
import           Model.Car
import           Parse.Csv
import           Util

instance FromCsvRow Car where
  fromCsvRow row = case row of
    [ manufacturer, model, ownerName, year, milage, color, hadAccidents, usedOrNew, price] -> Car
      <$> pure manufacturer
      <*> pure model
      <*> fromCsv ownerName
      <*> fromCsv year
      <*> fromCsv milage
      <*> fromCsv color
      <*> fromCsv hadAccidents
      <*> fromCsv usedOrNew
      <*> fromCsv price
    _ -> Left $ "Couldn't parse Car: incorrect number of fields: " <> tshow row