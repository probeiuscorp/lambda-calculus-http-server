module Main (main) where

import Text.Parsec
import LambdaCalculus.Parse (parserLambdaCalculus, parseModule, toNormalForm)
import LambdaCalculus.Interpret (evaluate, execute)
import Control.Monad (forever, forM, void, unless)
import qualified Data.Map as Map
import System.Environment (getArgs)
import System.Exit (exitFailure)

repl :: IO ()
repl = forever $ do
  line <- getLine
  print $ parse parserLambdaCalculus "-" line

main :: IO ()
main = do
  fileNames <- getArgs
  if null fileNames
    then repl
    else do
      sTermByIdentifier <- forM fileNames $ \fileName -> do
        fileContents <- readFile fileName
        let (errors, moduleScope) = parseModule fileName fileContents
        unless (null errors) $ print errors *> exitFailure
        pure moduleScope
      let allGlobals = (`foldMap` sTermByIdentifier) $ fmap $ evaluate allGlobals . toNormalForm
      print $ Map.keysSet allGlobals
      void $ execute $ allGlobals Map.! "main"
