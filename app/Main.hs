{-#LANGUAGE BlockArguments, OverloadedStrings, LambdaCase, NoMonomorphismRestriction#-}
module Main where

import Relude
import Relude.Extra

import System.Console.Haskeline

import Text.Parsec (ParseError)

import FShell.Parser
import Types
import FShell.Runtime
import FShell.NativeFs
import Lib
import Control.Error


main :: IO ()
main =  runPrompt initialShellState
    where
        runPrompt :: ShellState -> IO ()
        runPrompt s = runStateT (runExceptT (runInputT defaultSettings (forever prompt))) s `catch` replExceptionHandler s >>= \case
            (Left EOF, _) -> return ()
            (Left (ParseError perr), st) ->  errLn ("Parse Error: " <> show perr) >> runPrompt st
            (Left (VarNotFoundError v), st) -> errLn ("Variable not found: " <> v) >> runPrompt st
            (Left (NotAFunctionError nf), st) -> errLn ("Tried to call a value that is not a function: " <> show nf) >> runPrompt st
            (Left (TypeError v te), st) -> errLn ("Type Error: Expected type " <> te <> " but got: " <> show v) >> runPrompt st
            (Left (ArgumentMisMatchError msg args), st) -> errLn ("Argument Mismatch: " <> msg <> " (Arguments: " <> show args <> ")") >> runPrompt st
            (Left (LanguageError msg), st) -> errLn ("Language Error! Please report this: " <> msg) >> runPrompt st
            (Left (IOError msg), st) -> errLn ("IO Error: " <> msg) >> runPrompt st
            (Right _, st) -> void $ errLn $ "unexpexted end of shell loop\nlast shell state: \n" <> show st
        prompt :: Repl ()
        prompt = do
            inp <- fmap toText $ lift . (noteT' EOF) =<< getInputLine promptText
            ast <- stateM $ \s -> first' ParseError $ parse s statements "SHELL" inp
            mapM_ runStatement ast
            return ()
        replExceptionHandler :: ShellState -> SomeException -> IO (Either ShellExit (), ShellState)
        replExceptionHandler s e = return (Left (IOError (show e)), s)


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
