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
statement = import_ <|> def <|> Call <$> expr <?> "statement"

import_ :: Parser Statement
import_ = (fmap Import $ reserved "import" *>
    fmap (T.intercalate "/") (lexeme ((stringLiteral <|> identifier) `sepBy1` (char '/')))) <?> "import declaration"

def :: Parser Statement
def = try (Def <$> (identifier <|> parens operator) <*> many identifier <* symbol "=") <*> expr <?> "definition"

expr :: Parser Expr
expr = (fmap mkOpCall $ (Expr <$> exprNoOp)
        `chainl1`
        (operator >>= \o -> getOpPriority o >>= \p -> return \x y -> Op o p x y)) <?> "expression"
        where
            mkOpCall :: OpCallTree -> Expr
            mkOpCall = foldOpTree . iterateWhileDiff repairOpTree

            foldOpTree :: OpCallTree -> Expr
            foldOpTree = \case
                Expr e -> e
                Op o _ l r -> FCall (FCall (Var o) (foldOpTree l)) (foldOpTree r)

            repairOpTree :: OpCallTree -> OpCallTree
            repairOpTree = \case
                Expr e -> Expr e
                Op o p (Expr le) r -> Op o p (Expr le) r
                Op o (p, rassoc) (Op lo (lp, lrassoc) ll lr) r
                    | p > lp || p == lp && rassoc && lrassoc -> repairOpTree $ Op lo (lp, lrassoc) ll (Op o (p, rassoc) lr r) -- rotation
                    | otherwise -> Op o (p, rassoc) (repairOpTree (Op lo (lp, lrassoc) ll lr)) r

            getOpPriority :: Text -> Parser (Int, Bool)
            getOpPriority op = getState >>= \s ->
                maybe
                    (fail ("Operator not found '" ++ toString op ++ "'\nMaybe you forgot to declare its precedence?"))
                    return
                    (lookup op $ opPriorities s)

data OpCallTree
    = Expr Expr
    | Op Text (Int, Bool) OpCallTree OpCallTree
    deriving (Show, Eq)


exprNoOp :: Parser Expr
exprNoOp = (Var <$> identifier <|> nofcallexpr) >>= \initial -> foldl' FCall initial <$> (nofcallexpr `sepBy` spaces)

nofcallexpr :: Parser Expr
nofcallexpr = parens expr <|> progSubst <|> stringlit <|> boollit <|> numlit <|> listlit <|> lambda <|> ifthenelse <|> flag {-<|> path-} <|> var

progSubst :: Parser Expr
progSubst = FCall (Var "_runProg") <$> braces expr

stringlit :: Parser Expr
stringlit = ifParseMode ShellParse
    (StringLit <$> (stringLiteral <|> identifier))
    (StringLit <$> stringLiteral) <?> "string literal"

boollit :: Parser Expr
boollit = (fmap BoolLit $ reserved "True" $> True <|> reserved "False" $> False) <?> "boolean literal"

numlit :: Parser Expr
numlit = naturalOrFloat <&> either (NumLit . fromInteger) NumLit <?> "number literal"

listlit :: Parser Expr
listlit = ListLit <$> brackets (commaSep expr) <?> "list literal"

lambda :: Parser Expr
lambda = symbol "\\" >> Lambda <$> identifier <* symbol "->" <*> expr <?> "lambda expression"

ifthenelse :: Parser Expr
ifthenelse = reserved "if" >> If <$> expr <* reserved "then" <*> expr <* reserved "else" <*> expr <?> "if expression"

var :: Parser Expr
var = whenShell (symbol "$") >> Var <$> identifier <?> "variable"

pragma :: Parser (Maybe a)
pragma = (symbol "%" *> choice [modePragma] <|> opPriorityPragma) $> Nothing <?> "pragma"
    where
        modePragma :: Parser ()
        modePragma = do
            symbol "MODE" >> choice [ try $ symbol "SHELL"  $> ShellParse
                                    , symbol "SCRIPT" $> ScriptParse] 
                                    >>= \x -> modifyState (\s -> s{parseMode=x})
        opPriorityPragma :: Parser ()
        opPriorityPragma = do
            lr <- (reserved "infixl" $> False) <|> (reserved "infixr" $> True)
            priority <- fromInteger <$> natural
            op <- operator
            modifyState (\s -> s{opPriorities = opPriorities s & insert op (priority, lr)})


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

