module LambdaCalculus.Parse (parserLambdaCalculus) where

import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator
import Control.Applicative
import qualified Text.Parsec.Token as T
import qualified Data.List.NonEmpty as NE
import Text.Parsec.Language (emptyDef)
import Data.List.NonEmpty (some1)

data Term
  = TermIdentifier String
  | TermAbstraction (NE.NonEmpty String) Term
  | TermApplication Term Term 
  deriving (Eq, Ord, Show)

data NormalForm
  = NFIdentifier String
  | NFAbstraction NormalForm NormalForm
  | NFApplication String NormalForm
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
