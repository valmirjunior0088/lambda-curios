{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Equal
  ( MonadEqual (..)
  , EqualT (..)
  , runEqualT
  )
  where

import Term (Term, open)
import qualified Term as Term

import Util ((<==>), (.&&.), (.||.))
import Context (MonadContext, ContextT, access, fresh, reduce)

import Control.Monad.Trans (lift)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, asks, local)

class MonadEqual m where
  equal :: Term -> Term -> m Bool

newtype EqualT m a =
  EqualT (ReaderT [(Term, Term)] (ContextT m) a)
  deriving (Functor, Applicative, Monad, MonadReader [(Term, Term)])

runEqualT :: Monad m => EqualT m a -> ContextT m a
runEqualT (EqualT action) = runReaderT action []

instance Monad m => MonadContext (EqualT m) where
  access action = EqualT (lift $ access action)
  fresh = EqualT (lift fresh)
  reduce term = EqualT (lift $ reduce term)

remember :: Monad m => (Term, Term) -> EqualT m Bool -> EqualT m Bool
remember equation action = local (equation :) action

isEqual :: Monad m => (Term, Term) -> EqualT m Bool
isEqual (one, other) = return (one == other)

isRemembered :: Monad m => (Term, Term) -> EqualT m Bool
isRemembered equation = asks (elem equation)

isEquirecursive :: Monad m => (Term, Term) -> EqualT m Bool
isEquirecursive equation = remember equation $ case equation of
  (Term.FunctionType input scope, Term.FunctionType input' scope') -> do
    name <- fresh

    let
      output = open name scope
      output' = open name scope'

    equal input input' .&&. equal output output'

  (Term.Function body, Term.Function body') -> do
    name <- fresh

    let
      output = open name body
      output' = open name body'

    equal output output'
  
  (Term.Apply function argument, Term.Apply function' argument') ->
    equal function function' .&&. equal argument argument'
  
  (Term.PairType input scope, Term.PairType input' scope') -> do
    name <- fresh

    let
      output = open name scope
      output' = open name scope'

    equal input input' .&&. equal output output'
  
  (Term.Pair left right, Term.Pair left' right') ->
    equal left left' .&&. equal right right'
  
  (Term.Split scrutinee body, Term.Split scrutinee' body') -> do
    left <- fresh
    right <- fresh

    let
      output = open right (open left body)
      output' = open right (open left body')

    equal scrutinee scrutinee' .&&. equal output output'

  (Term.Match scrutinee branches, Term.Match scrutinee' branches') -> do
    let
      labelsAreEqual = map fst branches <==> map fst branches'

      contains (label, target) targets = case lookup label targets of
        Nothing -> return False
        Just body -> equal body target

      bodiesAreEqual = and <$> mapM (`contains` branches) branches'

    equal scrutinee scrutinee' .&&. pure labelsAreEqual .&&. bodiesAreEqual
  
  (_, _) -> return False

instance Monad m => MonadEqual (EqualT m) where
  equal one other = do
    equation <- (,) <$> reduce one <*> reduce other
    isEqual equation .||. isRemembered equation .||. isEquirecursive equation
