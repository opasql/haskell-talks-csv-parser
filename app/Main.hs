module Main where

import           Data.Text
import qualified Data.Text.IO as Text.IO
import           Model.Person
import qualified Parse.Csv    as Csv
import           Util

main :: IO ()
main = do
  input <- Text.IO.readFile "persons.csv"
  case Csv.parse input :: Either Text [Person] of
    Left e        -> Text.IO.putStrLn $ "Parsing failure: " <> e
    Right persons -> Text.IO.putStrLn $ tshow persons
