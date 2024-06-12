module Parse.Csv where

import           Control.Applicative
import qualified Data.Attoparsec.Text as Atto
import           Data.Functor         ((<&>))
import           Data.Text            hiding (filter)
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
    parseField = Atto.takeWhile (\c -> c /= ',' && c /= '\n')

parse :: FromCsvRow a => Text -> Either Text [a]
parse input = parseRows input >>= traverse fromCsvRow

runParser :: Atto.Parser a -> Text -> Either Text a
runParser p i = case Atto.parseOnly p i of
  Left e  -> Left $ pack e
  Right a -> Right a
