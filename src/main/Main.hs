module Main(main) where

import Text.Megaparsec(parseTest)
import Parser

main :: IO ()
main = do
  parseTest parseStuff 
    "5 //comment\n\t\
  	\ '6' \"78\""
