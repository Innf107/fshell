{-#LANGUAGE BlockArguments, OverloadedStrings, LambdaCase#-}
{-# LANGUAGE TupleSections, ScopedTypeVariables, NamedFieldPuns#-}
module FShell.Runtime where

import Relude
import Relude.Extra hiding ((.~), (%~))

import Control.Lens hiding (head1, (<.>))
import Control.Monad
import Control.Monad.Except


import System.Console.Haskeline
import System.Environment
import System.Directory
import System.Process
import System.Exit
import System.FilePath
import System.IO hiding (print, putStr, putChar, putStrLn)

import qualified Data.Text as T

import FShell.Parser
import Types
import Lib


runStatement :: Statement -> Repl ()
runStatement = \case
    Def vname params ex -> do
        let fun = foldr Lambda ex params
        whenNotNull params \params1 ->
            modify (\s -> s{scope = let scope' = (scope s) & functionScope . at vname ?~ Function (head1 params1) (foldr Lambda ex (tail params1)) scope' in scope'})
        value <- eval $ fun
        modify (\s -> s{scope = scope s & exportedScope . at vname ?~ value & functionScope .~ mempty})

    Call e -> eval e >>= \case
        Program p -> setNativeVar "status" . NumV . convert . exitNum . fst =<< callProgram True p
        UnitV -> pass
        v -> outputStrLn $ show v
    Import carry modName -> importModule carry modName

eval :: Expr -> Repl Value
eval = \case
    Unit -> return UnitV
    NumLit n -> return $ NumV n
    StringLit s -> return $ StringV s
    BoolLit b -> return $ BoolV b
    Path segs -> return $ PathV segs
    Flag f -> return $ FlagV f
    Var v -> do 
        s <- gets scope
        mv :: Maybe Value <- return (asumMap (lookup v) ([_functionScope s, _exportedScope s] ++ elems (_importedScope s) ++ [_nativeScope s]))
            <<|>> liftIO (fmap (StringV . toText) <$> lookupEnv (toString v))
            <<|>> liftIO (fmap (\p -> Program $ ProgramFragment p [] Nothing) <$> findProgramInPath v)
        lift $ noteT' (VarNotFoundError v) mv
    ListLit es -> ListV <$> mapM eval es
    Lambda x body -> Function x body <$> gets scope
    If condEx thEx elEx -> do
        cond <- eval condEx
        case cond of
            (BoolV True) -> eval thEx
            (BoolV False) -> eval elEx
            _ -> lift $ throwError (TypeError cond "Bool")
    FCall fex aex -> do
        f <- eval fex
        a <- eval aex
        case f of
            Function param body closure -> do
                prevScopes <- gets scope
                modify (\s -> s{scope=closure & functionScope . at param ?~ a})
                x <- eval body
                modify (\s -> s{scope=prevScopes})
                return x
            Program pf -> return $ Program pf{fragmentArgs = fragmentArgs pf ++ valToArgs a}
            NativeF 1 nf -> nf [a]
            NativeF argCount nf -> return $ NativeF (argCount - 1) (\xs -> nf (a:xs))
            _ -> lift $ throwError $ NotAFunctionError f

callProgram :: (MonadIO m) => Bool -> ProgramFragment -> m (ExitCode, String)
callProgram = liftIO .- callProgramInner Nothing
    where
    callProgramInner :: Maybe Handle -> Bool -> ProgramFragment -> IO (ExitCode, String) -- TODO: return more than just the exit code
    callProgramInner inStream toStdout ProgramFragment{fragmentPath, fragmentArgs, fragmentOutput} = case fragmentOutput of
        Nothing -> withCreateProcess (
            (proc fragmentPath (map toString fragmentArgs))
                {std_in=maybe Inherit UseHandle inStream, std_out=if toStdout then Inherit else CreatePipe})
            \_sin sout _serr processHandle ->
                (,) <$> waitForProcess processHandle <*> (fromMaybe (return "NOSTDOUT") (hGetContentsStrict <$> sout))
        Just outProg -> do
            withCreateProcess ((proc fragmentPath (map toString fragmentArgs)){std_in=maybe Inherit UseHandle inStream, std_out=CreatePipe})
                        \_sin sout _serr processHandle -> do
                            res <- callProgramInner sout toStdout outProg
                            _ <- waitForProcess processHandle
                            return res

valToArgs :: Value -> [Text]
valToArgs = \case
    BoolV b -> [show b]
    NumV n -> [show n]
    NativeF i f -> [show (NativeF i f)]
    Function a e s -> [show (Function a e s)]
    Program fp -> ["PROGRAM(" <> toText (fragmentPath fp) <> ")"] --TODO
    StringV s -> [s]
    ListV vs -> concatMap valToArgs vs
    UnitV -> ["()"]
    PathV [""] -> ["/"]
    PathV segs -> [T.intercalate "/" segs]
    FlagV f -> [f]

setNativeVar :: Name -> Value -> Repl ()
setNativeVar vname val = modify (\s -> s{scope=scope s & nativeScope . at vname ?~ val})


importModule :: Bool -> Name -> Repl ()
importModule carry modName = do
    -- TODO search in other paths ($FSHELLPATH ?)
    -- TODO: use paths relative to the current module (for nested imports)
    parseEnv <- getParseEnv
    
    let modPath = toString $ modName <> ".fsh"
    modFile <- liftIO $ readFileText modPath <|> readFileText (fshellpath parseEnv </> modPath)
    prevParseMode <- gets parseMode
    
    modify (\s -> s{parseMode=ScriptParse})

    modAst <- stateM $ \s -> case parse s parseEnv statements modPath modFile of
        Left e -> modify (\s'->s'{parseMode=prevParseMode}) >> throwError (ImportError modName (ParseError e))
        Right res -> return res
        
    prevScope <- gets scope

    modify \s -> s{scope=scope s & exportedScope .~ mempty & importedScope .~ mempty}

    mapM_ runStatement modAst `catchError` \e -> do
        modify \s -> s{parseMode=prevParseMode, scope=prevScope}
        throwError (ImportError modName e)
        
    exported <- gets $ _exportedScope . scope
    modify \s -> s{parseMode=prevParseMode, scope = prevScope & importedScope %~ (insert modName exported)}
    when (carry) $ modify \s -> s{scope = scope s & exportedScope %~ (<> exported)}


findProgramInPath :: Name -> IO (Maybe FilePath)
findProgramInPath prog = do
    pathVars <- map (toString . (<>"/"<>prog)) <$> T.split (==':') . toText <$> getEnv "PATH"
    candidates <- filterM doesFileExist pathVars
    return $ viaNonEmpty head candidates


getParseEnv :: (MonadIO m) => m ParseEnv
getParseEnv = do
    homeDirectory <- liftIO getHomeDirectory
    fshellpath <- liftIO $ fromMaybe (homeDirectory </> ".fshell") <$> lookupEnv "FSHELLPATH"
    return $ ParseEnv {
              homeDirectory
            , fshellpath
        }

