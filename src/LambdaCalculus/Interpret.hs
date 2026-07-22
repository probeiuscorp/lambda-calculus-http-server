module LambdaCalculus.Interpret (evaluate, execute) where

import LambdaCalculus.Parse
import qualified Data.Map as Map

newtype Value = Value (Value -> Value)
type Scope = Map.Map String Value
evaluate :: Scope -> NormalForm -> Value
evaluate scope = \case
  NFIdentifier str -> scope Map.! str
  NFApplication nfFn arg -> let (Value fn) = evaluate scope nfFn in
    fn $ evaluate scope arg
  NFAbstraction param body -> Value $ \arg -> evaluate (Map.insert param arg scope) body

execute :: Value -> IO Value
execute v = v <$ print "executing function!"
