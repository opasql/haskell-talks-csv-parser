module Model.Car where

import           Data.Text

data Car = Car
  { carManufacturer :: Text
    -- ^ Can't be an empty string
  , carModel        :: Text
    -- ^ Can't be an empty string
  , carOwnerName    :: Maybe Text
    -- ^ This one can be omitted
  , carYear         :: Year
    -- ^ A proper range for a year would be 1900 - 2025
  , carMilage       :: Milage
    -- ^ Milage can't be negative
  , carColor        :: Color
  , carHadAccidents :: HadAccidents
  , carUsedOrNew    :: UsedOrNew
  , carPrice        :: Price
    -- ^ Price can't be negative
  } deriving Show

newtype Year = Year { unYear :: Int }
  deriving Show

newtype Milage = Milage { unMilage :: Double }
  deriving Show

newtype Price = Price { unPrice :: Double }
  deriving Show

newtype HadAccidents = HadAccidents { unHadAccidents :: Bool }
  deriving Show

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
