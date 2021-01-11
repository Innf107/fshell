{-#LANGUAGE BlockArguments, OverloadedStrings, LambdaCase#-}
module Main where

import Relude
import Relude.Extra

import System.Console.Haskeline
import System.Directory
import System.FilePath

import Text.Parsec (ParseError)

import FShell.Parser
import Types hiding (Parser)
import FShell.Runtime
import FShell.NativeFs
import Lib
import Control.Error
import Control.Monad.Catch
import Control.Monad.Except

import Options.Applicative

import qualified Data.Text as T

data RunArguments = RunArguments {
        runType :: RunType
    }

data RunType = RunSimple | RunFile FilePath | RunExpr Text deriving (Show, Eq)

parseArgs :: ParserInfo RunArguments
parseArgs = info (argParser <**> helper)
    (header "fshell - A functional shell")
    where
        argParser :: Parser RunArguments
        argParser = RunArguments <$>
            (runExpr <|> runSimple <|> runFile)

        runExpr = RunExpr <$> strOption (long "exec" <> short 'c' <> metavar "expression")
        runSimple = pure RunSimple
        runFile = RunFile <$> strArgument (metavar "file")

main :: IO ()
main = execParser parseArgs >>= \runArgs -> case runType runArgs of
    RunSimple -> runPrompt initialShellState
    RunFile fp -> putStrLn $ "Run file " <> fp
    RunExpr ex -> putTextLn $ "Run expression: " <> ex

runPrompt :: ShellState -> IO ()
runPrompt s = do
    home <- getHomeDirectory
    let haskelineSettings = defaultSettings{historyFile = Just (home </> ".fshell" </> "history")}

    runStateT (runExceptT (runInputT haskelineSettings
        (importStdlib >> forever ((prompt `catch` replExceptionHandler) `catchError` errorHandler)))) s >>= \case
        (Left EOF, _) -> return ()
        (Right _, st) -> void $ errLn $ "unexpexted end of shell loop\nlast shell state: \n" <> show st
        (Left e, st) -> void $ errLn $ "Unexpected end of shell. This is either a bug or an issue with your stdlib file. \n(" 
            <> showError e <> ")\n\nlast shell state: " <> show st
    where
        importStdlib :: Repl ()
        importStdlib = runStatement (Import False "stdlib/stdlib")
        prompt :: Repl ()
        prompt = do
            promptText <- getPromptText
            inp <- fmap toText $ lift . (noteT' EOF) =<< getInputLine promptText
            parseEnv <- getParseEnv
            ast <- stateM $ \st -> first' ParseError $ parse st parseEnv statements "SHELL" inp
            mapM_ runStatement ast
            return ()
        replExceptionHandler :: SomeException -> Repl ()
        replExceptionHandler e = throwError (IOError (show e))
        errorHandler :: ShellExit -> Repl ()
        errorHandler = \case
            EOF -> throwError EOF
            e -> outputLn (showError e)

showError :: ShellExit -> Text
showError = \case
    EOF -> "EOF"
    (ParseError perr) -> "Parse Error: " <> show perr
    (VarNotFoundError v) -> "Variable not found: " <> v
    (NotAFunctionError nf) -> "Tried to call a value that is not a function: " <> show nf
    (TypeError v te) -> "Type Error: Expected type " <> te <> " but got: " <> show v
    (ArgumentMisMatchError msg args) -> "Argument Mismatch: " <> msg <> " (Arguments: " <> T.intercalate ", " (map show args) <> ")"
    (IOError msg) -> "IO Error: " <> msg
    (LanguageError msg) -> "Language Error! Please report this: " <> msg
    (ImportError modname er) -> "Error importing module '" <> modname <> "': " <> showError er
    (ScriptError msg) -> "A Script had an error: " <> msg
    
getPromptText :: Repl String
getPromptText = eval (FCall (Var "prompt") Unit) `catchError` (const (pure UnitV)) <&> \case
    StringV t -> toString t
    _ -> "PROMPTERROR$ "


initialShellState :: ShellState
initialShellState = ShellState {
          parseMode=ShellParse
        , scope=Scope {
              _nativeScope=nativeFs
            , _importedScope=mempty
            , _exportedScope=mempty
            , _functionScope=mempty
        }
        , opPriorities=mempty
    }
