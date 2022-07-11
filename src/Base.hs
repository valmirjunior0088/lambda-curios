{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Base
  ( MonadBase (..)
  , BaseT
  , runBaseT
  )
  where

import Term (unwrap, Term, instantiate)
import qualified Term as Term

import Context (Context)
import qualified Context as Context

import Util ((!?))

import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, ask)
import Control.Monad.State (MonadState, StateT, evalStateT, get, put)

class Monad m => MonadBase m where
  askGlobals :: m Context
  getLocals :: m Context
  putLocals :: Context -> m ()
  fresh :: m String
  reduce :: Term -> m Term

newtype BaseT m a =
  BaseT (StateT (Context, Integer) (ReaderT Context m) a)
  deriving (Functor, Applicative, Monad, MonadReader Context, MonadState (Context, Integer))

runBaseT :: Monad m => BaseT m a -> Context -> m a
runBaseT (BaseT action) globals = runReaderT (evalStateT action (Context.empty, 0)) globals

instance Monad m => MonadBase (BaseT m) where
  askGlobals = ask

  getLocals = do
    (locals, _) <- get
    return locals

  putLocals locals = do
    (_, source) <- get
    put (locals, source)

  fresh = do
    (locals, source) <- get
    put (locals, succ source)
    return (show source)
  
  reduce = \case
    Term.Global name -> do
      globals <- askGlobals
      
      case Context.definition name globals of
        Just term -> reduce term
        _ -> return (Term.Global name)
    
    Term.Local variable -> do
      locals <- getLocals
      
      case Context.definition (unwrap variable) locals of
        Just term -> reduce term
        _ -> return (Term.Local variable)

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
