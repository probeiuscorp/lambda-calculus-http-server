module LambdaCalculus.Interpret (intrinsics, evaluate, execute) where

import LambdaCalculus.Parse
import qualified Data.Map as Map
import Data.List (intercalate)

data Value
  = Intrinsic Intrinsic
  | L (Value -> Value)
instance Show Value where
  show (Intrinsic intrinsic) = show intrinsic
  show (L _) = "<lambda>"
app :: Value -> Value -> Value
app (L fn) arg = fn arg
app (Intrinsic lhs) (Intrinsic rhs) = Intrinsic $ appIntrinsic lhs rhs
app lhs rhs = error $ unwords ["type error, evaluating intrinsic:", show lhs, show rhs] 
infixl `app`
type Scope = Map.Map String Value
evaluate :: Scope -> NormalForm -> Value
evaluate scope = \case
  NFIdentifier ident -> case Map.lookup ident scope of
    Just val -> val
    Nothing -> error $ mconcat
      [ "identifier not in scope: "
      , ident
      , "\n  Scope of evaluation:\n     "
      , intercalate ", " $ Map.keys scope
      ]
  NFApplication nfFn arg -> evaluate scope nfFn `app` evaluate scope arg
  NFAbstraction param body -> L $ \arg -> evaluate (Map.insert param arg scope) body

data Intrinsic
  = IFn (Intrinsic -> Intrinsic)
  | Nat Int
instance Show Intrinsic where
  show (IFn _) = "<function>"
  show (Nat n) = "Nat(" <> show n <> ")"
appIntrinsic :: Intrinsic -> Intrinsic -> Intrinsic
appIntrinsic (IFn fn) rhs = fn rhs
appIntrinsic _ _ = error "type error: expected function"

intrinsics :: Scope
intrinsics = mempty

execute :: Value -> IO Value
execute program = case evaluated of
  Intrinsic (Nat n) -> L id <$ print n
  val -> error $ "type error: was expecting int, got " <> show val
  where
    evaluated = program
      `app` Intrinsic (IFn $ \(Nat n) -> Nat (n + 1))
      `app` Intrinsic (Nat 0)
