module Main where

import           Data.Text
import qualified Data.Text.IO as Text.IO
import           Model.Car
import           Model.Person
import qualified Parse.Csv    as Csv
import           Parse.Oleg   ()
import           Util

main :: IO ()
main = do
  olegMain
  input <- Text.IO.readFile "persons.csv"
  case Csv.parse input :: Either Text [Person] of
    Left e        -> Text.IO.putStrLn $ "Parsing failure: " <> e
    Right persons -> Text.IO.putStrLn $ tshow persons

olegMain :: IO ()
olegMain = do
  input <- Text.IO.readFile "cars.csv"
  case Csv.parse input :: Either Text [Car] of
    Left e        -> Text.IO.putStrLn $ "Parsing failure: " <> e
    Right persons -> Text.IO.putStrLn $ tshow persons
