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

data IOKind = IOPure | IOFlatMap | IOGetInt | IOPutInt | IOGetLine | IOPutStrLn
  deriving (Eq, Ord, Show)
data Intrinsic
  = IFn (Intrinsic -> Intrinsic)
  | INat Int
  | IList [Value]
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
  [ ("io_pure", L $ \v -> Intrinsic $ IIO IOPure [v])
  , ("io_flatMap", L $ \ma -> L $ \f -> Intrinsic $ IIO IOFlatMap [ma, f])
  , ("io_getInt", Intrinsic $ IIO IOGetInt [])
  , ("io_putInt", L $ \int -> Intrinsic $ IIO IOPutInt [int])
  , ("io_getLine", Intrinsic $ IIO IOGetLine [])
  , ("io_putStrLn", L $ \str -> Intrinsic $ IIO IOPutStrLn [str])
  ]

execute :: Value -> IO Value
execute = \case
  Intrinsic (IIO IOPure [v]) -> pure v
  Intrinsic (IIO IOFlatMap [io, f]) -> do
    execute . app f =<< execute io
  Intrinsic (IIO IOGetInt []) -> do
    toChurchInt . read <$> getLine
  Intrinsic (IIO IOPutInt [vint]) -> do
    L id <$ print (fromChurchInt vint)
  Intrinsic (IIO IOGetLine []) -> do
    toChurchList . fmap toChurchChar <$> getLine
  Intrinsic (IIO IOPutStrLn [vline]) -> do
    print vline
    L id <$ putStrLn (fromChurchChar <$> fromChurchList vline)
  val -> error $ "unmatched IO: " <> show val

toChurchInt :: Int -> Value
toChurchInt 0 = L $ const $ L id
toChurchInt n = L $ \f -> L $ \a -> f `app` (toChurchInt (n - 1) `app` f `app` a)
fromChurchInt :: Value -> Int
fromChurchInt val = let
  subject = val
    `app` L (\(Intrinsic (INat n)) -> Intrinsic (INat (n + 1)))
    `app` Intrinsic (INat 0)
  in case subject of
    Intrinsic (INat n) -> n
    evalled -> error $ "type error: was expecting int, got " <> show evalled

toChurchChar :: Char -> Value
toChurchChar = toChurchInt . fromEnum
fromChurchChar :: Value -> Char
fromChurchChar = toEnum . fromChurchInt

toChurchList :: [Value] -> Value
toChurchList [] = L $ const $ L id
toChurchList (x:xs) = L $ \ifCons -> L $ \_ -> ifCons `app` x `app` toChurchList xs
fromChurchList :: Value -> [Value]
fromChurchList val = let
  subject = (val
    `app` L (\x -> L (\xs ->
        Intrinsic $ IList (x : fromChurchList xs)
      )))
    `app` Intrinsic (IList [])
  in case subject of
    Intrinsic (IList list) -> list
    evalled -> error $ "type error: was expecting list, got " <> show evalled
