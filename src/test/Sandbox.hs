module Main(main) where

-- import Text.Megaparsec (parseTest)
import Text.Megaparsec hiding (State)
import Parser.Internal

main :: IO ()
main = do
  putStrLn "-- sandbox"
  parseTest (exprP) "larry"
  parseTest (exprP) "x / y * z"
