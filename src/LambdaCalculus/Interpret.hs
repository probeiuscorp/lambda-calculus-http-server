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

data IOKind = IOGetInt | IOPutInt | IOPure | IOFlatMap
  deriving (Eq, Ord, Show)
data Intrinsic
  = IFn (Intrinsic -> Intrinsic)
  | INat Int
  | IList [Int]
  | IIO IOKind [Value]
instance Show Intrinsic where
  show (IFn _) = "<function>"
  show (INat n) = "Nat(" <> show n <> ")"
  show (IList nats) = "List[" <> intercalate "," (show <$> nats) <> "]"
  show (IIO kind positions) = "IO(" <> show kind <> ")[" <> show (length positions) <> "]"
appIntrinsic :: Intrinsic -> Intrinsic -> Intrinsic
appIntrinsic (IFn fn) rhs = fn rhs
appIntrinsic _ _ = error "type error: expected function"

intrinsics :: Scope
intrinsics = Map.fromList
  [ ("io_getInt", Intrinsic $ IIO IOGetInt [])
  , ("io_putInt", L $ \int -> Intrinsic $ IIO IOPutInt [int])
  , ("io_pure", L $ \v -> Intrinsic $ IIO IOPure [v])
  , ("io_flatMap", L $ \ma -> L $ \f -> Intrinsic $ IIO IOFlatMap [ma, f])
  ]

execute :: Value -> IO Value
execute = \case
  Intrinsic (IIO IOGetInt []) -> do
    Intrinsic . INat . read <$> getLine
  Intrinsic (IIO IOPutInt [vint]) -> do
    L id <$ print (fromChurchInt vint)
  Intrinsic (IIO IOPure [v]) -> pure v
  Intrinsic (IIO IOFlatMap [io, f]) -> do
    execute . app f =<< execute io
  val -> error $ "umatched IO: " <> show val

fromChurchInt :: Value -> Int
fromChurchInt val = let
  subject = val
    `app` Intrinsic (IFn $ \(INat n) -> INat (n + 1))
    `app` Intrinsic (INat 0)
  in case subject of
    Intrinsic (INat n) -> n
    evalled -> error $ "type error: was expecting int, got " <> show evalled