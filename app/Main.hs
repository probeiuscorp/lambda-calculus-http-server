module Main (main) where

import Text.Parsec
import LambdaCalculus.Parse (parserLambdaCalculus, parseModule, toNormalForm)
import LambdaCalculus.Interpret (evaluate, execute)
import Control.Monad (forever, forM, void)
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
        let smTerm = parseModule fileContents $ parse parserLambdaCalculus fileName
        forM smTerm $ \case
          Right term -> pure term
          Left err -> print err *> exitFailure
      let allGlobals = (`foldMap` sTermByIdentifier) $ fmap $ evaluate allGlobals . toNormalForm
      void $ execute $ allGlobals Map.! "main"
