{-# LANGUAGE LambdaCase, BlockArguments, OverloadedStrings #-}
module Types where

import Relude
import Relude.Extra
import qualified Data.Text as T

import System.Console.Haskeline (InputT)

import qualified GHC.Show as Show (Show(..))

import Text.Parsec (ParsecT, ParseError)

type Name = Text

data Statement = Def Name [Name] Expr
               | Call Expr
               | Import Bool Name
               deriving (Show, Eq)

data Expr = Var Name
          | FCall Expr Expr
          | Lambda Name Expr
          | If Expr Expr Expr
          | NumLit Double
          | StringLit Text
          | BoolLit Bool
          | Path [Text]
          | Flag Text
          | ListLit [Expr]
          | Unit
          deriving (Show, Eq)


data Value = BoolV Bool
           | StringV Text
           | ListV [Value]
           | NumV Double
           | PathV [Text]
           | FlagV Text
           | NativeF Int ([Value] -> Repl Value)
           | Function Name Expr Scope
           | Program ProgramFragment
           | UnitV

data ProgramFragment = ProgramFragment {
          fragmentPath :: FilePath
        , fragmentArgs :: [Text]
        , fragmentOutput :: Maybe ProgramFragment
    }
    deriving (Show, Eq)

instance Show Value where
    show = \case
        BoolV x -> show x
        StringV x -> show x
        ListV vs -> show vs
        NumV x -> show x
        PathV [""] -> "/"
        PathV ps -> toString $ T.intercalate "/" ps
        FlagV f -> toString f
        NativeF i _ -> "<NATIVE FUNCTION(" ++ show i ++ ")>"
        Function _ _ _ -> "<FUNCTION>"
        Program fp -> show fp
        UnitV -> "()"

instance Eq Value where
    (==) = curry \case
        (BoolV x, BoolV y) -> x == y
        (StringV x, StringV y) -> x == y
        (ListV xs, ListV ys) -> xs == ys
        (NumV x, NumV y) -> x == y
        (Function x1 y1 z1, Function x2 y2 z2) -> x1 == x2 && y1 == y2 && z1 == z2
        (Program x, Program y) -> x == y
        (_, _) -> False


type ScopeFrame = HashMap Name Value

type ModuleName = Name

data Scope = Scope {
          _nativeScope::ScopeFrame
        , _importedScope::HashMap ModuleName ScopeFrame
        , _exportedScope::ScopeFrame
        , _functionScope::ScopeFrame
    }
    deriving (Show, Eq)


-- Lenses defined manually because template haskell would break mutual structural recursion
nativeScope :: Lens' Scope ScopeFrame
nativeScope = lens _nativeScope (\i x -> i{_nativeScope=x})
importedScope :: Lens' Scope (HashMap ModuleName ScopeFrame)
importedScope = lens _importedScope (\i x -> i{_importedScope=x})
exportedScope :: Lens' Scope ScopeFrame
exportedScope = lens _exportedScope (\i x -> i{_exportedScope=x})
functionScope :: Lens' Scope ScopeFrame
functionScope = lens _functionScope (\i x -> i{_functionScope=x})


data ParseEnv = ParseEnv {
          homeDirectory :: FilePath
        , fshellpath :: FilePath
    } deriving (Show, Eq)

type Parser = ParsecT Text ShellState (Reader ParseEnv)

data ParseMode = ShellParse
               | ScriptParse
               deriving (Show, Eq)

data ShellState = ShellState {
        parseMode :: ParseMode
      , scope :: Scope
      --                                   rightassociative
      , opPriorities :: HashMap Name (Int, Bool)
    } deriving (Show, Eq)

data ShellExit = EOF
               | ParseError ParseError
               | VarNotFoundError Name
               | NotAFunctionError Value
               | ArgumentMisMatchError Text [Value]
               | LanguageError Text
               | TypeError Value Name
               | IOError Text
               | ImportError ModuleName ShellExit
               deriving (Show, Eq)

type Repl a = InputT (ExceptT ShellExit (StateT ShellState IO)) a


