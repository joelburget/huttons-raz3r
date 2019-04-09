{-# language GADTs                      #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase                 #-}
module Hutton where

import Control.Monad.Error.Class
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map
import Data.SBV
import qualified Data.SBV.Internals as SBVI

data Expr ty where
  Add :: Expr Integer -> Expr Integer  -> Expr Integer
  Ite :: Expr Bool -> Expr a -> Expr a -> Expr a
  Lit :: SymVal a => a                 -> Expr a
  Var :: String                        -> Expr a

newtype Eval a = Eval { runEval :: ReaderT (Map String SBVI.SVal) (Either String) a }
  deriving (Functor, Applicative, Monad, MonadReader (Map String SBVI.SVal),
    MonadError String)

instance Mergeable a => Mergeable (Eval a) where
  symbolicMerge force test left right = Eval $ ReaderT $ \env -> do
    -- Note we're in the `Either String` monad and if either side fails the
    -- whole computation does
    left'  <- runReaderT (runEval left)  env
    right' <- runReaderT (runEval right) env
    pure $ symbolicMerge force test left' right'

eval :: SymVal a => Expr a -> Eval (SBV a)
eval = \case
  Add x y -> (+) <$> eval x <*> eval y
  Ite a b c -> do
    a' <- eval a
    ite a' (eval b) (eval c)
  Lit i -> pure $ literal i
  Var v -> do
    env <- ask
    case Map.lookup v env of
      Nothing   -> throwError $ "variable not found: " ++ v
      Just sval -> pure $ SBVI.SBV sval
