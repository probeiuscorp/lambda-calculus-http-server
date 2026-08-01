module Main (main) where

import Text.Parsec
import LambdaCalculus.Parse (parserLambdaCalculus, parseModule, toNormalForm)
import LambdaCalculus.Interpret (evaluate, execute, intrinsics)
import LambdaCalculus.Verify (isNotWellOrdered)
import Control.Monad (forever, forM, forM_, void, unless)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Data.List (intercalate)

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
      let globalScope = foldMap (fmap toNormalForm) sTermByIdentifier
      forM_ (isNotWellOrdered globalScope) $ \path -> do
        putStrLn $ "Cyclic definitions: " <> intercalate " -> " (NE.toList $ path <> pure (NE.head path))
        exitFailure
      let allGlobals = (<> intrinsics) $ (`foldMap` sTermByIdentifier) $ fmap $ evaluate allGlobals . toNormalForm
      void $ execute $ allGlobals Map.! "main"
