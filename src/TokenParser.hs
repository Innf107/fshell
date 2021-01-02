module TokenParser where

import Relude hiding ((<|>))

import Text.Parsec as P hiding (State)
import qualified Text.Parsec.Token as PT
import Text.Parsec.Language

import Control.Lens

import qualified Data.Text as T
import Data.Text (Text)

import Data.Maybe

import Types


import Types

fshellDef :: GenLanguageDef Text ShellState (Reader ParseEnv)
fshellDef = PT.LanguageDef {
          PT.commentLine = "#"
        , PT.commentStart = "{-"
        , PT.commentEnd = "-}"
        , PT.nestedComments = True

        , PT.caseSensitive = True
        , PT.reservedNames = ["if", "then", "else", "True", "False", "import", "carry", "infixl", "infixr"]
        , PT.reservedOpNames = []

        , PT.identStart = letter <|> oneOf "_"
        , PT.identLetter = alphaNum <|> oneOf "_'?-"

        , PT.opStart = PT.opLetter fshellDef
        , PT.opLetter = oneOf ":!#%&*+./<=>@\\^|-~"
    }


tp :: PT.GenTokenParser Text ShellState (Reader ParseEnv)
tp = PT.makeTokenParser fshellDef

identifier :: Parser Text
identifier = T.pack <$> PT.identifier tp

reserved :: Text -> Parser ()
reserved = PT.reserved tp . T.unpack

operator :: Parser Text
operator = T.pack <$> PT.operator tp

reservedOp :: Text -> Parser ()
reservedOp = PT.reservedOp tp . T.unpack

charLiteral :: Parser Char
charLiteral = PT.charLiteral tp

stringLiteral :: Parser Text
stringLiteral = T.pack <$> PT.stringLiteral tp

natural :: Parser Integer
natural = PT.natural tp

float :: Parser Double
float = PT.float tp

naturalOrFloat :: Parser (Either Integer Double)
naturalOrFloat = PT.naturalOrFloat tp

decimal :: Parser Integer
decimal = PT.decimal tp

hexadecimal :: Parser Integer
hexadecimal = PT.hexadecimal tp

octal :: Parser Integer
octal = PT.octal tp

symbol :: Text -> Parser Text
symbol = fmap T.pack . PT.symbol tp . T.unpack

lexeme :: Parser a -> Parser a
lexeme = PT.lexeme tp

whiteSpace :: Parser ()
whiteSpace = PT.whiteSpace tp

parens :: Parser a -> Parser a
parens = PT.parens tp

braces :: Parser a -> Parser a
braces = PT.braces tp

angles :: Parser a -> Parser a
angles = PT.angles tp

brackets :: Parser a -> Parser a
brackets = PT.brackets tp

semi :: Parser Text
semi = T.pack <$> PT.semi tp

comma :: Parser Text
comma = T.pack <$> PT.comma tp

colon :: Parser Text
colon = T.pack <$> PT.colon tp

dot :: Parser Text
dot = T.pack <$> PT.dot tp

semiSep :: Parser a -> Parser [a]
semiSep = PT.semiSep tp

semiSep1 :: Parser a -> Parser [a]
semiSep1 = PT.semiSep1 tp

commaSep :: Parser a -> Parser [a]
commaSep = PT.commaSep tp

commaSep1 :: Parser a -> Parser [a]
commaSep1 = PT.commaSep1 tp



