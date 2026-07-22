module Main (main) where

import Text.Parsec
import LambdaCalculus.Parse (parserLambdaCalculus)
import Control.Monad (forever)

main :: IO ()
main = forever $ do
  line <- getLine
  print $ parse parserLambdaCalculus "-" line
