module Main where

import           Data.Text
import qualified Data.Text.IO       as Text.IO
import           Model.Car
import           Model.Person
import qualified Parse.Csv          as Csv
import           Parse.Valeri
import           Text.Pretty.Simple
import           Util

main :: IO ()
main =
  pPrint "Persons Example: " >> mainPersons >> pPrint "\nLera: " >> mainLera

mainPersons :: IO ()
mainPersons = do
  input <- Text.IO.readFile "persons.csv"
  case Csv.parse input :: Either Text [Person] of
    Left e        -> Text.IO.putStrLn $ "Parsing failure: " <> e
    Right persons -> pPrint persons

mainLera :: IO ()
mainLera = do
  input <- Text.IO.readFile "cars.csv"
  case Csv.parse input :: Either Text [Car] of
    Left e     -> Text.IO.putStrLn $ "Parsing failure: " <> e
    Right cars -> pPrint cars
