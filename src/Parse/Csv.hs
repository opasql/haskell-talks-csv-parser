{-# LANGUAGE FlexibleInstances #-}

module Parse.Csv where

import           Control.Applicative
import qualified Data.Attoparsec.Text as Atto
import           Data.Functor         ((<&>))
import           Data.Text            hiding (filter)
import           Model.Car
import           Prelude

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

-- FIXME:
-- 1. Переиспользовать инстанс Double
-- 2. Валидировать Price
instance FromCsv Price where
  fromCsv t = case fromCsv t :: Either Text Double of
    Left e -> Left e
    Right d ->
      if d < 0
      then Left $ "Negative price: " <> t
      else Right $ Price d

-- FIXME:
-- 1. Переиспользовать инстанс Bool
-- Type inference
instance FromCsv HadAccidents where
  fromCsv = fmap HadAccidents . fromCsv

-- Не нужно выносить логику
-- parseBool :: Atto.Parser Bool
-- parseBool = (trueP <|> falseP) <* Atto.endOfInput
--   where
--     trueP = Atto.asciiCI "true" *> pure True
--     falseP = Atto.asciiCI "false" *> pure False

-- FIXME:
-- 1. Переиспользовать инстанс Double
-- 2. Валидировать Milage
instance FromCsv Milage where
  fromCsv = fmap Milage . runParser (Atto.double <* Atto.endOfInput)

-- FIXME:
-- 1. Переиспользовать инстанс Double
-- 2. Валидировать Year
instance FromCsv Year where
  fromCsv = fmap Year . runParser (Atto.signed Atto.decimal <* Atto.endOfInput)

instance FromCsv Color where
  fromCsv = runParser parseColor

parseColor :: Atto.Parser Color
parseColor = Atto.choice (fmap f [minBound..maxBound])
  <|> (Atto.takeText >>= \col -> fail $ "Unknown color: " ++ unpack col)
  where
    f color = Atto.asciiCI (renderColor color) *> pure color

instance FromCsv UsedOrNew where
  fromCsv = runParser parseUsedOrNew

parseUsedOrNew :: Atto.Parser UsedOrNew
parseUsedOrNew = Atto.choice
  [ Atto.asciiCI "used" *> pure Used
  , Atto.asciiCI "new" *> pure New
  ] <|> (Atto.takeText >>= \uon -> fail $ "Unknown UsedOrNew: " ++ unpack uon)

-- instance FromCsv (Maybe Text) where
--   fromCsv txt
--     | Data.Text.strip txt == "" = Right Nothing
--     | otherwise = Right (Just txt)

instance FromCsv a => FromCsv (Maybe a) where
  fromCsv "" = Right Nothing
  fromCsv t  = Just <$> fromCsv t
