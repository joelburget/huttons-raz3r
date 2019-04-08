{-# language LambdaCase #-}
module Hutton where

import Control.Monad.Reader
import Data.Map (Map)
import Data.SBV

data Expr
  = Add Expr Expr
  | Lit Integer

type Eval = ReaderT (Map String (SBV Integer)) (Either String)

eval :: Expr -> Eval (SBV Integer)
eval = \case
  Add x y -> (+) <$> eval x <*> eval y
  Lit i   -> pure $ literal i
