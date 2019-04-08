{-# language LambdaCase #-}
module Hutton where

import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map
import Data.SBV

data Expr
  = Add Expr Expr
  | Lit Integer
  | Var String

type Eval = ReaderT (Map String (SBV Integer)) (Either String)

eval :: Expr -> Eval (SBV Integer)
eval = \case
  Add x y -> (+) <$> eval x <*> eval y
  Lit i   -> pure $ literal i
  Var v   -> do
    env <- ask
    case Map.lookup v env of
      Nothing  -> lift $ Left $ "variable not found: " ++ v
      Just val -> pure val
