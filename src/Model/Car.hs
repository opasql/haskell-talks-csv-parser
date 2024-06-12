{-# LANGUAGE LambdaCase #-}

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

-- Car :: Text -> Text -> Maybe Text -> Year -> Milage

newtype Year = Year { unYear :: Int }
  deriving Show

-- | Milage is a non-negative double value.
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
  | Rainbow
  deriving (Show, Eq, Ord, Enum, Bounded)

renderColor :: Color -> Text
renderColor = \case
  White   -> "white"
  Black   -> "black"
  Red     -> "red"
  Orange  -> "orange"
  Yellow  -> "yellow"
  Green   -> "green"
  Blue    -> "blue"
  Purple  -> "purple"
  Brown   -> "brown"
  Rainbow -> "rainbow"

data UsedOrNew = Used | New
  deriving Show
