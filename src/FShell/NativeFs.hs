{-# LANGUAGE LambdaCase, BlockArguments, OverloadedStrings #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}

module FShell.NativeFs (nativeFs) where

import Relude

import Control.Error (throwE)

import qualified Data.Text as T

import System.Environment
import System.IO
import System.Directory

import FShell.Runtime
import Types
import Lib

nativeFs :: HashMap Name Value
nativeFs = fromList [
        -- Generic
          _typeof
        , f2 "_eq" (return .- BoolV .- (==))

        -- Numbers
        , numF2 "_addNum" (NumV .- (+))
        , numF2 "_subNum" (NumV .- (-))
        , numF2 "_mulNum" (NumV .- (*))
        , numF2 "_divNum" (NumV .- (/))
        , numF2 "_divIntNum" (NumV .- fromIntegral @ Integer .- round .- (/))
        , numF2 "_modNum" (\x y -> NumV $ convert $ (floor x :: Int) `mod` (floor y :: Int))
        , numF2 "_ltNum" (BoolV .- (<))

        -- Lists
        , _isnillist
        , _conslist
        , _headlist
        , _taillist

        -- Strings
        , stringF2 "_stringappend" (StringV .- (<>))
        , stringF1 "_stringchars" (ListV . map (StringV . T.singleton) . toString)

        -- Shell
        , ("status", NumV 0)
        , _runProg
        , _setEnv
        , _cd
        , _pipe
    ]


_typeof :: (Text, Value)
_typeof = f1 "_typeof" $ return . StringV . \case
    BoolV _ -> "Bool"
    StringV _ -> "String"
    ListV _ -> "List"
    NumV _ -> "Num"
    NativeF _ _ -> "NativeF"
    Function _ _ _ -> "Function"
    Program _ -> "Program"

_isnillist :: (Text, Value)
_isnillist = f1 "_isnillist" \case
    ListV x -> return $ BoolV $ x == []
    x -> argTypeError "_isnillist" "List" [x]

_conslist :: (Text, Value)
_conslist = f2 "_conslist" $ curry \case
    (x, ListV xs) -> return (ListV (x:xs))
    (_, x) -> argTypeError "_conslist" "List" [x]

_headlist :: (Text, Value)
_headlist = f1 "_headlist" \case
    ListV (x:_) -> return x
    ListV [] -> lift $ throwE $ ArgumentMisMatchError "_headlist<native>: Empty list" []
    x -> argTypeError "_headlist" "List" [x]

_taillist :: (Text, Value)
_taillist = f1 "_taillist" \case
    ListV (_:xs) -> return $ ListV xs
    ListV [] -> lift $ throwE $ ArgumentMisMatchError "_taillist<native>: Empty list" []
    x -> argTypeError "_taillist" "List" [x]

_runProg :: (Text, Value)
_runProg = f1 "_runProg" \case
    Program pf -> do 
        (status, out) <- callProgram False pf
        setNativeVar "status" (NumV $ convert $ exitNum status)
        return $ StringV (toText out)
    x -> argTypeError "_runProg" "ProgramFragment" [x]

_setEnv :: (Text, Value)
_setEnv = f2 "_setEnv" $ curry \case
    (StringV var, val) -> liftIO $ setEnv (toString var) (toString $ fromMaybe "" $ viaNonEmpty head $ valToArgs val) $> StringV var
    (x,_) -> argTypeError "_setEnv" "String" [x]

_cd :: (Text, Value)
_cd = f1 "_cd" $ \case
    StringV var -> liftIO $ setCurrentDirectory (toString var) >> (getCurrentDirectory >>= setEnv "PWD") $> StringV var
    x -> argTypeError "_setEnv" "String or Path" [x]

_pipe :: (Text, Value)
_pipe = f2 "_pipe" $ curry \case
    (Program p1, Program p2) -> return $ Program $ setProgramOutput p1 p2
    (x, y) -> argTypeError "_pipe" "Program" [x, y]
    where
        setProgramOutput :: ProgramFragment -> ProgramFragment -> ProgramFragment
        setProgramOutput p1@ProgramFragment{fragmentOutput} p2 = case fragmentOutput of
            Nothing -> p1{fragmentOutput=Just p2}
            Just p3 -> p1{fragmentOutput=Just (setProgramOutput p3 p2)}
-- Helpers

f1 :: Text -> (Value -> Repl Value) -> (Text, Value)
f1 name f = (name,) $ NativeF 1 \case
    [x] -> f x
    xs -> lift $ throwE $ LanguageError $ name <> "<native>: expected exactly 1 argument [" <> show xs <> "]"

f2 :: Text -> (Value -> Value -> Repl Value) -> (Text, Value)
f2 name f = (name,) $ NativeF 2 \case
    [x, y] -> f x y
    xs -> lift $ throwE $ LanguageError $ name <> "<native>: expected exactly 2 arguments [" <> show xs <> "]"

numF2 :: Text -> (Double -> Double -> Value) -> (Text, Value)
numF2 name f = (name,) $ NativeF 2 \case
    [NumV x, NumV y] -> return (f x y)
    xs -> lift $ throwE $ ArgumentMisMatchError (name <> "<native>: expected exactly 2 numbers") xs

stringF1 :: Text -> (Text -> Value) -> (Text, Value)
stringF1 name f = (name,) $ NativeF 1 \case
    [StringV x] -> return (f x)
    xs -> lift $ throwE $ ArgumentMisMatchError (name <> "<native>: expected exactly 1 string") xs

stringF2 :: Text -> (Text -> Text -> Value) -> (Text, Value)
stringF2 name f = (name,) $ NativeF 2 \case
    [StringV x, StringV y] -> return (f x y)
    xs -> lift $ throwE $ ArgumentMisMatchError (name <> "<native>: expected exactly 2 strings") xs

argTypeError :: Name -> Text -> [Value] -> Repl a
argTypeError funname expectedType xs = lift $ throwE $ ArgumentMisMatchError
    (funname <> "<native>: expected type " <> expectedType) xs

