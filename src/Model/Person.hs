module Model.Person where

import           Data.Text
import           Parse.Csv
import           Util

data Person = Person
  { personFirstName :: Text
  , personLastName  :: Text
  , personAge       :: Int
  , personHeight    :: Double
  } deriving Show

instance FromCsvRow Person where
  fromCsvRow row = case row of
    [firstNameRaw, lastNameRaw, ageRaw, heightRaw] -> Person
      <$> fromCsv firstNameRaw
      <*> fromCsv lastNameRaw
      <*> fromCsv ageRaw
      <*> fromCsv heightRaw
    _ -> Left $ "Couldn't parse Person: incorrect number of fields: " <> tshow row
