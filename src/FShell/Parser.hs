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

import System.Directory

import TokenParser
import Types
import Lib

parse :: ShellState -> ParseEnv -> Parser a -> FilePath -> Text -> Either ParseError (a, ShellState)
parse st env p filename input = flip runReader env $ runParserT ((,) <$> p <*> getState <* eof) st filename input

statements :: Parser [Statement]
statements = whiteSpace *> ifParseMode ShellParse
            (maybeToList <$> pragma <|> pure <$> statement)
            (catMaybes <$> many (pragma <|> (Just <$> statement <* symbol ";")))
            <* eof

statement :: Parser Statement
statement = import_ <|> def <|> Call <$> expr <?> "statement"

import_ :: Parser Statement
import_ = (reserved "import" *> pure Import <*> (option False (reserved "carry" $> True)) <*>
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
exprNoOp = (path <|> flag <|> Var <$> identifier <|> nofcallexpr) >>= \initial -> foldl' FCall initial <$> (nofcallexpr `sepBy` spaces)

nofcallexpr :: Parser Expr
nofcallexpr = choice [
          parens expr, progSubst, boollit, numlit, listlit
        , lambda, ifthenelse, try var, try pathNoRoot, try flag, stringlit
    ]

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
flag = Flag . toText <$> lexeme do
    _ <- char '-'
    mm <- option "" (string "-")
    (("-" <> mm) <>) <$> flagChars
    where
        flagChars = many1 (alphaNum <|> oneOf "_-+=@")

--TODO: ~
path :: Parser Expr
path = (try pathNoRoot <|> string "/" $> Path [""]) <?> "path"

pathNoRoot :: Parser Expr
pathNoRoot = whiteSpace >> (Path . map toText <$> lexeme (homePath <|> pathLeadingSlash <|> try regularPath <|> singleSegWithDot))
    where
        homePath :: Parser [String]
        homePath = do
            _ <- char '~'
            slash
            rest <- pathSeg `sepEndBy1` slash
            home <- asks homeDirectory
            return (map toString (T.split (=='/') (toText home)) ++ rest)
        pathLeadingSlash = do
            slash
            ([""] <>) <$> (pathSeg `sepEndBy1` slash)
        regularPath = do
            seg1 <- pathSeg
            slash
            (seg1:) <$> pathSeg `sepEndBy` slash
        singleSegWithDot = pathSeg >>= \x -> guard ('.' `elem` x) $> [x]
        pathSeg = many1 $ alphaNum <|> oneOf "_-#."
        slash = void (many1 $ string "/")


ifParseMode :: ParseMode -> Parser a -> Parser a -> Parser a
ifParseMode m x y = getState <&> parseMode >>= \pm -> if pm == m then x else y

whenParseMode :: ParseMode -> Parser a -> Parser () 
whenParseMode m p = ifParseMode m (void p) pass 

whenScript :: Parser a -> Parser ()
whenScript = whenParseMode ScriptParse

whenShell :: Parser a -> Parser ()
whenShell = whenParseMode ShellParse

