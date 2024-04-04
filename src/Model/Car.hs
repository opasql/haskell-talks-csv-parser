module Model.Car where

import           Data.Text
import           Parse.Csv
import           Util

data Car = Car
  { carManufacturer :: Text
  , carModel        :: Text
  , carOwnerName    :: Maybe Text
  , carYear         :: Int
  , carMilage       :: Double
  , carColor        :: Color
  , carHadAccidents :: Bool
  , carUsedOrNew    :: UsedOrNew
  , carPrice        :: Double
  } deriving Show

data Color =
    White
  | Black
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Purple
  | Brown
  deriving Show

data UsedOrNew = Used | New
  deriving Show
