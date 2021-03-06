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

data BinaryOp a where
  Add :: BinaryOp Integer
  Sub :: BinaryOp Integer
  Mul :: BinaryOp Integer
  Div :: BinaryOp Integer

  And :: BinaryOp Bool
  Or  :: BinaryOp Bool

data UnaryOp a where
  Neg :: UnaryOp Integer
  Not :: UnaryOp Bool

data Fun a b where
  Fun :: SymVal a => String -> Expr b -> Fun a b

data Expr ty where
  BinaryOp :: BinaryOp a -> Expr a -> Expr a -> Expr a
  UnaryOp  :: UnaryOp  a           -> Expr a -> Expr a
  Ite      :: Expr Bool -> Expr a  -> Expr a -> Expr a
  App      :: Fun a b -> Expr a              -> Expr b
  Lit      :: SymVal a => a                  -> Expr a
  Var      :: String                         -> Expr a

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
  BinaryOp op x y -> do
    let op' = case op of
          Add -> (+)
          Sub -> (-)
          Mul -> (*)
          Div -> sDiv
          And -> (.&&)
          Or  -> (.||)
    op' <$> eval x <*> eval y
  UnaryOp op x -> do
    let op' = case op of
          Neg -> negate
          Not -> sNot
    op' <$> eval x
  Ite a b c -> do
    a' <- eval a
    ite a' (eval b) (eval c)
  App (Fun varName body) arg -> do
    SBVI.SBV arg' <- eval arg
    local (Map.insert varName arg') (eval body)
  Lit i -> pure $ literal i
  Var v -> do
    env <- ask
    case Map.lookup v env of
      Nothing   -> throwError $ "variable not found: " ++ v
      Just sval -> pure $ SBVI.SBV sval
