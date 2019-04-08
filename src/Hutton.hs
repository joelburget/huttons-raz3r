{-# language LambdaCase #-}
module Hutton where

import Data.SBV

data Expr
  = Add Expr Expr
  | Lit Integer

eval :: Expr -> SBV Integer
eval = \case
  Add x y -> eval x + eval y
  Lit i   -> literal i
