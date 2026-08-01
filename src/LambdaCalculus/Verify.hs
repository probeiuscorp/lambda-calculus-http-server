module LambdaCalculus.Verify (isNotWellOrdered) where

import LambdaCalculus.Parse
import qualified Algebra.Graph as G
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Foldable as Set
import Algebra.Graph.ToGraph (ToGraph(topSort))
import Data.List.NonEmpty (NonEmpty)

findFree :: NormalForm -> Set.Set String
findFree = \case
  NFIdentifier ident -> Set.singleton ident
  NFApplication fn arg -> findFree fn <> findFree arg
  NFAbstraction parameter body -> Set.delete parameter $ findFree body

isNotWellOrdered :: Map.Map String NormalForm -> Maybe (NonEmpty String)
isNotWellOrdered scope = case z of
  Left path -> Just path
  Right _ -> Nothing
  where
    x = findFree <$> scope
    q = (`Map.foldMapWithKey` x) $ \binding freeSet -> Set.foldMap (binding `G.edge`) freeSet
    z = topSort q
