{-# language GADTs      #-}
{-# language LambdaCase #-}
module Hutton where

import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map
import Data.SBV
import qualified Data.SBV.Internals as SBVI

data Expr ty where
  Add :: Expr Integer -> Expr Integer -> Expr Integer
  Lit :: SymVal a => a                -> Expr a
  Var :: String                       -> Expr a

type Eval = ReaderT (Map String SBVI.SVal) (Either String)

eval :: Expr a -> Eval (SBV a)
eval = \case
  Add x y -> (+) <$> eval x <*> eval y
  Lit i   -> pure $ literal i
  Var v   -> do
    env <- ask
    case Map.lookup v env of
      Nothing   -> lift $ Left $ "variable not found: " ++ v
      Just sval -> pure $ SBVI.SBV sval
