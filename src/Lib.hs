{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
module Lib where

import Relude
import Relude.Extra

import System.Directory
import System.IO
import System.Console.Haskeline
import System.Exit

import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM


import Control.Monad.Error

import Data.Maybe (fromJust)
import Data.Text (Text, pack)

whenMonoidM :: (Monad m, Monoid (m a)) => m Bool -> m a -> m a
whenMonoidM mc ma = mc >>= \case
    True -> ma
    False -> mempty

withMonoidMaybe :: (Monoid m) => Maybe a -> (a -> m) -> m
withMonoidMaybe Nothing _ = mempty
withMonoidMaybe (Just x) f = f x

whenMaybeM :: (Monad m) => m Bool -> m (Maybe a) -> m (Maybe a)
whenMaybeM mc ma = mc >>= \case
    True -> ma
    False -> return Nothing

showT :: (Show x) => x -> Text
showT = pack . show

lookupJ :: (Ord k) => k -> Map k v -> v
lookupJ = (fromJust .) . M.lookup

removeDir :: FilePath -> IO Bool
removeDir fp = doesDirectoryExist fp >>= \case
    False -> return False
    True -> removeDirectoryRecursive fp >> return True

{-
instance (MonadFail m) => MonadFail (InputT m) where
    fail = lift . fail
-}

instance (MonadError e m) => MonadError e (InputT m) where
    throwError = lift . throwError
    catchError x f = withRunInBase (\unlift -> unlift x `catchError` (unlift . f))

--withRunInBase :: Monad m => ((forall a . InputT m a -> m a) -> m b) -> InputT m b

{-
instance (MonadException m) => MonadException (ExceptT e m) where
    controlIO f = ExceptT $ controlIO $ \(RunIO run) -> let
                    run' = RunIO (fmap ExceptT . run . runExceptT)
                    in fmap runExceptT $ f run'
-}

instance (MonadState s m) => MonadState s (InputT m) where
    state = lift . state
{-
class MonadIO m => MonadException m where
    controlIO :: (RunIO m -> IO (m a)) -> m a
newtype RunIO m = RunIO (forall b . m b -> IO (m b))

controlIO :: (RunIO (IO a) -> IO (IO a)) -> IO a
controlIO :: ((forall b. IO b -> IO (IO b)) -> IO (IO a)) -> IO a

controlIO :: (RunIO (ExceptT e m b) -> IO (m a)) -> ExceptT e m a
controlIO :: ((forall b. ExceptT e m b -> IO (ExceptT e m b)) -> IO (ExceptT e m a)) -> ExceptT e m a

-}

putStdError :: String -> IO ()
putStdError = hPutStrLn stderr

noteT' :: (Monad m) => e -> Maybe a -> ExceptT e m a
noteT' e = maybeToExceptT e . hoistMaybe

first' :: (Monad m) => (a -> c) -> Either a b -> ExceptT c m b
first' f = ExceptT . return . first f 

(<<>>) :: (Semigroup a, Applicative f) => f a -> f a -> f a
(<<>>) = liftA2 (<>) 

(<<|>>) :: (Alternative a, Applicative f) => f (a b) -> f (a b) -> f (a b)
(<<|>>) = liftA2 (<|>)

stateM :: (MonadState s (m n), MonadTrans m, Monad n) => (s -> n (a, s)) -> m n a
stateM f = do
    s <- get
    (a, s') <- lift $ f s
    put s'
    return a
    
mapFirst :: (a -> a) -> [a] -> [a]
mapFirst f (x:xs) = f x : xs
mapFirst _ [] = error "mapFst on empty list"
    

(.-) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.-) = (.) . (.)
infixr 9 .-

exitNum :: ExitCode -> Int
exitNum = \case
    ExitSuccess -> 0
    ExitFailure x -> x

hGetContentsStrict :: Handle -> IO String
hGetContentsStrict h = hGetContents h >>= \s -> length s `seq` return s

class (Num a, Num b) => NumConv a b where
    convert :: a -> b

instance (Integral a) => NumConv a Integer where
    convert = toInteger

instance (Num a) => NumConv Integer a where
    convert = fromInteger

instance (Integral a, Num b) => NumConv a b where
    convert = fromInteger . toInteger


iterateWhileDiff :: Eq a => (a -> a) -> a -> a
iterateWhileDiff f x
    | f x == x = x
    | otherwise = iterateWhileDiff f (f x)
