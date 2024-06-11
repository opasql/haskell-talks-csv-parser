{-# LANGUAGE FlexibleInstances #-}

module Parse.Csv where

import           Control.Applicative
import qualified Data.Attoparsec.Text as Atto
import           Data.Functor         ((<&>))
import           Data.Text            hiding (filter)
import           Prelude
import           Model.Car

class FromCsv a where
  fromCsv :: Text -> Either Text a

instance FromCsv Text where
  fromCsv = Right

instance FromCsv Int where
  fromCsv = runParser (Atto.signed Atto.decimal <* Atto.endOfInput)

instance FromCsv Double where
  fromCsv = runParser (Atto.double <* Atto.endOfInput)

instance FromCsv Bool where
  fromCsv = runParser $ (trueP <|> falseP) <* Atto.endOfInput
    where
      trueP = Atto.asciiCI "true" *> pure True
      falseP = Atto.asciiCI "false" *> pure False

class FromCsvRow a where
  fromCsvRow :: [Text] -> Either Text a

class ToCsv a where
  toCsv :: a -> Text

class ToCsvRow a where
  toCsvRow :: a -> [Text]

parseRows :: Text -> Either Text [[Text]]
parseRows input = runParser parseLines input <&> \a ->
  filter (\as -> as /= [""] && as /= []) a
  where
    parseLines :: Atto.Parser [[Text]]
    parseLines = Atto.sepBy parseLine (Atto.char '\n') <* Atto.endOfInput

    parseLine :: Atto.Parser [Text]
    parseLine = Atto.sepBy parseField (Atto.char ',')

    parseField :: Atto.Parser Text
    parseField = Atto.choice
      [ Atto.takeWhile (\c -> c /= ',' && c /= '\n')
      , pure ""
      ]

parse :: FromCsvRow a => Text -> Either Text [a]
parse input = parseRows input >>= traverse fromCsvRow

runParser :: Atto.Parser a -> Text -> Either Text a
runParser p i = case Atto.parseOnly p i of
  Left e  -> Left $ pack e
  Right a -> Right a


-- Valeri's changes

instance FromCsv Price where
  fromCsv = fmap Price . runParser (Atto.double <* Atto.endOfInput)

instance FromCsv HadAccidents where
  fromCsv = fmap HadAccidents . runParser parseBool

parseBool :: Atto.Parser Bool
parseBool = (trueP <|> falseP) <* Atto.endOfInput
  where
    trueP = Atto.asciiCI "true" *> pure True
    falseP = Atto.asciiCI "false" *> pure False

instance FromCsv Milage where
  fromCsv = fmap Milage . runParser (Atto.double <* Atto.endOfInput)

instance FromCsv Year where
  fromCsv = fmap Year . runParser (Atto.signed Atto.decimal <* Atto.endOfInput)

instance FromCsv Color where
  fromCsv = runParser parseColor

parseColor :: Atto.Parser Color
parseColor = Atto.choice
  [ Atto.asciiCI "white" *> pure White
  , Atto.asciiCI "black" *> pure Black
  , Atto.asciiCI "red" *> pure Red
  , Atto.asciiCI "orange" *> pure Orange
  , Atto.asciiCI "yellow" *> pure Yellow
  , Atto.asciiCI "green" *> pure Green
  , Atto.asciiCI "blue" *> pure Blue
  , Atto.asciiCI "purple" *> pure Purple
  , Atto.asciiCI "brown" *> pure Brown
  ] <|> (Atto.takeText >>= \col -> fail $ "Unknown color: " ++ unpack col)

instance FromCsv UsedOrNew where
  fromCsv = runParser parseUsedOrNew

parseUsedOrNew :: Atto.Parser UsedOrNew
parseUsedOrNew = Atto.choice
  [ Atto.asciiCI "used" *> pure Used
  , Atto.asciiCI "new" *> pure New
  ] <|> (Atto.takeText >>= \uon -> fail $ "Unknown UsedOrNew: " ++ unpack uon)

instance FromCsv (Maybe Text) where
  fromCsv txt
    | Data.Text.strip txt == "" = Right Nothing
    | otherwise = Right (Just txt)
