module Main(main) where

import Parser

main :: IO ()
main = do
  putStrLn "-- parse test"
  parseTest shellP "5 + 6"
