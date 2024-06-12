{-# LANGUAGE LambdaCase #-}

module Parse.Oleg where

import           Model.Car
import           Parse.Csv
import           Util

instance FromCsv Year where
  fromCsv t = case fromCsv t of
    Right i ->
      if i < 1900 || i > 2025
      then Left $ "Invalid Year: " <> t
      else Right $ Year i
    Left e -> Left e

instance FromCsv Milage where
  fromCsv t = case fromCsv t of
    Right d ->
      if d < 0
      then Left $ "Invalid Milage: " <> t
      else Right $ Milage d
    Left e -> Left e

instance FromCsv Price where
  fromCsv t = case fromCsv t of
    Right d ->
      if d < 0
      then Left $ "Invalid Price: " <> t
      else Right $ Price d
    Left e -> Left e

instance FromCsv HadAccidents where
  fromCsv = fmap HadAccidents . fromCsv

instance FromCsv Color where
  fromCsv = \case
   "white"  -> pure White
   "black"  -> pure Black
   "red"    -> pure Red
   "orange" -> pure Orange
   "yellow" -> pure Yellow
   "green"  -> pure Green
   "blue"   -> pure Blue
   "purple" -> pure Purple
   "brown"  -> pure Brown
   t        -> Left $ "Invalid Color: " <> t

instance FromCsv UsedOrNew where
  fromCsv = \case
   "used" -> pure Used
   "new"  -> pure New
   t      -> Left $ "Invalid Used or New: " <> t

instance FromCsvRow Car where
  fromCsvRow = \case
    [ manufacturerRaw, modelRaw, ownerRaw, yearRaw, milageRaw, colorRaw
      , hadAccidentsRaw, usedOrNewRaw, priceRaw ] -> Car
        <$> fromCsv manufacturerRaw
        <*> fromCsv modelRaw
        <*> fromCsv ownerRaw
        <*> fromCsv yearRaw
        <*> fromCsv milageRaw
        <*> fromCsv colorRaw
        <*> fromCsv hadAccidentsRaw
        <*> fromCsv usedOrNewRaw
        <*> fromCsv priceRaw
    row -> Left $ "Couldn't parse Car: incorrect number of fields: " <> tshow row

instance FromCsv a => FromCsv (Maybe a) where
  fromCsv "" = pure Nothing
  fromCsv t  = Just <$> fromCsv t
