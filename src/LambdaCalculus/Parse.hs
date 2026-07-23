module LambdaCalculus.Parse (parserLambdaCalculus, parseModule, NormalForm(..), toNormalForm) where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator
import Control.Applicative
import qualified Text.Parsec.Token as T
import qualified Data.List.NonEmpty as NE
import Text.Parsec.Language (emptyDef)
import Data.List (groupBy)
import Data.List.NonEmpty (some1)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Either (partitionEithers)
import Data.Char (isSpace)

data Term
  = TermIdentifier String
  | TermAbstraction (NE.NonEmpty String) Term
  | TermApplication Term Term 
  deriving (Eq, Ord, Show)

data NormalForm
  = NFIdentifier String
  | NFAbstraction String NormalForm
  | NFApplication NormalForm NormalForm
  deriving (Eq, Ord, Show)

lexerDef :: T.TokenParser ()
lexerDef = T.makeTokenParser emptyDef
identifier = T.identifier lexerDef
literal = T.lexeme lexerDef . satisfy . (==)

parserLambdaCalculus :: Parser Term
parserLambdaCalculus = asum
  [ uncurry TermAbstraction <$> do
    literal 'λ'
    params <- some1 identifier
    literal '.'
    body <- parserLambdaCalculus
    return (params, body)
  , foldl1 TermApplication <$> many1 simple
  , simple
  ]

simple :: Parser Term
simple = asum
  [ literal '(' *> parserLambdaCalculus <* literal ')'
  , TermIdentifier <$> identifier
  ]

toNormalForm :: Term -> NormalForm
toNormalForm = \case
  TermIdentifier str -> NFIdentifier str
  TermApplication fn arg -> NFApplication (toNormalForm fn) (toNormalForm arg)
  TermAbstraction params body -> foldr NFAbstraction (toNormalForm body) params

parserDeclaration :: Parser (String, Term)
parserDeclaration = do
  binding <- identifier
  literal '='
  term <- parserLambdaCalculus
  pure (binding, term)

parseModule :: String -> String -> ([ParseError], Map.Map String Term)
parseModule fileName fileContents = (parseErrors, Map.fromList declarations)
  where
    nextLineHasLeadingWhitespace = const $ \case
      (' ':_) -> True
      _ -> False
    declarationSources = groupBy nextLineHasLeadingWhitespace (lines fileContents) >>= \linesGroup ->
      ([unlines linesGroup | not (all (all isSpace) linesGroup)])
    (parseErrors, declarations) = partitionEithers $ parse parserDeclaration fileName <$> declarationSources
