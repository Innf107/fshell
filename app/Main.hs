{-#LANGUAGE BlockArguments, OverloadedStrings, LambdaCase#-}
module Main where

import Relude
import Relude.Extra

import System.Console.Haskeline
import System.Directory
import System.FilePath

import Text.Parsec (ParseError)

import FShell.Parser
import Types
import FShell.Runtime
import FShell.NativeFs
import Lib
import Control.Error
import Control.Exception (catch)

import qualified Data.Text as T

main :: IO ()
main = runPrompt initialShellState
    where
        runPrompt :: ShellState -> IO ()
        runPrompt s = do 
            home <- getHomeDirectory
            let haskelineSettings = defaultSettings{historyFile = Just (home </> ".fshell" </> "history")}
            
            runStateT (runExceptT (runInputT haskelineSettings (forever prompt))) s `catch` replExceptionHandler s >>= \case
                (Left EOF, _) -> return ()
                (Left e, st) ->  errLn (showError e) >> runPrompt st
                (Right _, st) -> void $ errLn $ "unexpexted end of shell loop\nlast shell state: \n" <> show st
        prompt :: Repl ()
        prompt = do
            inp <- fmap toText $ lift . (noteT' EOF) =<< getInputLine promptText
            ast <- stateM $ \s -> first' ParseError $ parse s statements "SHELL" inp
            mapM_ runStatement ast
            return ()
        replExceptionHandler :: ShellState -> SomeException -> IO (Either ShellExit (), ShellState)
        replExceptionHandler s e = return (Left (IOError (show e)), s)

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


promptText :: String
promptText = "\1\ESC[34m\2Test\1\ESC[0m\2:\1\ESC[38;2;0;255;255m\2$\1\ESC[0m\2 "


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
