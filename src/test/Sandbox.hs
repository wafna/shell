module Main(main) where

import Data.Text
-- import Text.Megaparsec (parseTest)
import Text.Megaparsec hiding (State)
import Parser.Internal

testParse :: Show a => Parser a -> String -> IO()
testParse p s = do
  putStrLn "----------------------------------------------"
  putStrLn $ "-- " ++ s
  parseTest p $ pack s

main :: IO ()
main = do
  putStrLn "-- sandbox"
  -- parseTest (exprP) "x"
  testParse (exprP) "x / y * z"
  testParse (exprP) "(x / y) * z"
