{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Context
  ( MonadContext (..)
  , ContextT (..)
  , runContextT
  )
  where

import Term (Term, instantiate)
import qualified Term as Term

import Globals (Globals)
import qualified Globals as Globals

import Util ((!?))

import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, asks)
import Control.Monad.State (MonadState, StateT, evalStateT, get, put)

class MonadContext m where
  access :: (Globals -> a) -> m a
  fresh :: m String
  reduce :: Term -> m Term

newtype ContextT m a =
  ContextT (StateT Integer (ReaderT Globals m) a)
  deriving (Functor, Applicative, Monad, MonadReader Globals, MonadState Integer)

runContextT :: Monad m => ContextT m a -> Globals -> m a
runContextT (ContextT action) globals = runReaderT (evalStateT action 0) globals

instance Monad m => MonadContext (ContextT m) where
  access = asks

  fresh = do
    source <- get
    put (succ source)
    return (show source)
  
  reduce = \case
    Term.Global name -> access (Globals.definition name) >>= \case
      Just term -> reduce term
      _ -> return (Term.Global name)

    Term.Apply function argument -> reduce function >>= \case
      Term.Function body -> reduce (instantiate argument body)
      _ -> return (Term.Apply function argument)

    Term.Split scrutinee body -> reduce scrutinee >>= \case
      Term.Pair left right -> reduce (instantiate right (instantiate left body))
      _ -> return (Term.Split scrutinee body)

    Term.Match scrutinee branches -> reduce scrutinee >>= \case
      Term.Label label -> reduce (label !? branches)
      _ -> return (Term.Match scrutinee branches)
    
    term -> return term
