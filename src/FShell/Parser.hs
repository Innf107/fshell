{-# LANGUAGE RecordWildCards, OverloadedStrings, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
module FShell.Parser where

import Relude hiding ((<|>), many, optional, Op)

import Relude.Extra.Map

import Text.Parsec as P hiding (State)
import qualified Text.Parsec.Token as PT
import Text.Parsec.Language

import qualified Data.Text as T

import TokenParser
import Types
import Lib

parse :: ShellState -> Parser a -> FilePath -> Text -> Either ParseError (a, ShellState)
parse st p filename input = runParser ((,) <$> p <*> getState <* eof) st filename input

statements :: Parser [Statement]
statements = whiteSpace *> ifParseMode ShellParse
            (maybeToList <$> pragma <|> pure <$> statement)
            (catMaybes <$> many (pragma <|> (Just <$> statement <* symbol ";")))
            <* eof

statement :: Parser Statement
statement = import_ <|> def <|> Call <$> expr

import_ :: Parser Statement
import_ = fmap Import $ reserved "import" *>
    fmap (T.intercalate "/") (lexeme ((stringLiteral <|> identifier) `sepBy1` (char '/')))

def :: Parser Statement
def = try (Def <$> (identifier <|> parens operator) <*> many identifier <* symbol "=") <*> expr

expr :: Parser Expr
expr = fmap mkOpCall $ (Expr <$> exprNoOp)
        `chainl1`
        (operator >>= \o -> getOpPriority o >>= \p -> return \x y -> Op o p x y)
        where
            mkOpCall :: OpCallTree -> Expr
            mkOpCall = foldOpTree . repairOpTree

            foldOpTree :: OpCallTree -> Expr
            foldOpTree = \case
                Expr e -> e
                Op o _ l r -> FCall (FCall (Var o) (foldOpTree l)) (foldOpTree r)

            repairOpTree :: OpCallTree -> OpCallTree
            repairOpTree = \case
                Expr e -> Expr e
                Op o p (Expr le) r -> Op o p (Expr le) r
                Op o p (Op lo lp ll lr) r
                    | p > lp -> repairOpTree $ Op lo lp ll (Op o p lr r) -- rotation
                    | otherwise -> Op o p (repairOpTree (Op lo lp ll lr)) r

            getOpPriority :: Text -> Parser Int
            getOpPriority op = getState >>= \s ->
                maybe
                    (fail ("Operator not found '" ++ toString op ++ "'\nMaybe you forgot to declare its precedence?"))
                    return
                    (lookup op $ opPriorities s)

data OpCallTree
    = Expr Expr
    | Op Text Int OpCallTree OpCallTree
    deriving (Show, Eq)


exprNoOp :: Parser Expr
exprNoOp = (Var <$> identifier <|> nofcallexpr) >>= \initial -> foldl' FCall initial <$> (nofcallexpr `sepBy` spaces)

nofcallexpr :: Parser Expr
nofcallexpr = parens expr <|> stringlit <|> boollit <|> numlit <|> listlit <|> lambda <|> ifthenelse <|> flag {-<|> path-} <|> var

stringlit :: Parser Expr
stringlit = ifParseMode ShellParse
    (StringLit <$> (stringLiteral <|> identifier))
    (StringLit <$> stringLiteral)

boollit :: Parser Expr
boollit = fmap BoolLit $ reserved "True" $> True <|> reserved "False" $> False

numlit :: Parser Expr
numlit = naturalOrFloat <&> either (NumLit . fromInteger) NumLit

listlit :: Parser Expr
listlit = ListLit <$> brackets (commaSep expr)

lambda :: Parser Expr
lambda = symbol "\\" >> Lambda <$> identifier <* symbol "->" <*> expr

ifthenelse :: Parser Expr
ifthenelse = reserved "if" >> If <$> expr <* reserved "then" <*> expr <* reserved "else" <*> expr

var :: Parser Expr
var = whenShell (symbol "$") >> Var <$> identifier

pragma :: Parser (Maybe a)
pragma = (symbol "%" *> choice [modePragma] <|> opPriorityPragma) $> Nothing
    where
        modePragma :: Parser ()
        modePragma = do
            symbol "MODE" >> choice [ try $ symbol "SHELL"  $> ShellParse
                                    , symbol "SCRIPT" $> ScriptParse] 
                                    >>= \x -> modifyState (\s -> s{parseMode=x})
        opPriorityPragma :: Parser ()
        opPriorityPragma = do
            reserved "infixl"
            priority <- fromInteger <$> natural
            op <- operator
            modifyState (\s -> s{opPriorities = opPriorities s & insert op priority})


flag :: Parser Expr
flag = fail "Flag NYI" {-Flag . toText <$> lexeme do
    _ <- char '-'
    mm <- toText <$> option "" (string "-")
    (("-" <> mm) <>) <$> identifier
-}
--TODO
path :: Parser Expr
path = fail "Path NYI" --Path . map toText <$> lexeme do many1 (noneOf (" \t\n"::String)) `sepBy1` (string "/")


ifParseMode :: ParseMode -> Parser a -> Parser a -> Parser a
ifParseMode m x y = getState <&> parseMode >>= \pm -> if pm == m then x else y

whenParseMode :: ParseMode -> Parser a -> Parser () 
whenParseMode m p = ifParseMode m (void p) pass 

whenScript :: Parser a -> Parser ()
whenScript = whenParseMode ScriptParse

whenShell :: Parser a -> Parser ()
whenShell = whenParseMode ShellParse

