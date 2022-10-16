module Main(main) where

import Text.Megaparsec(parseTest)
import Parser

main :: IO ()
main = do
  parseTest parseStuff "5 + 6 //comment\n\t\
  	\"
