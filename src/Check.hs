{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Check
  ( checks
  )
  where

import Term (Variable (Variable), Type, Term, instantiate, open)
import qualified Term as Term

import Globals (Globals)
import qualified Globals as Globals

import Locals (Locals)
import qualified Locals as Locals

import Util (unique, (<==>))
import Context (MonadContext, ContextT, runContextT, access, fresh, reduce)
import Equal (MonadEqual, runEqualT, equal)

import Control.Monad (unless)
import Control.Monad.Trans (lift)
import Control.Monad.Except (MonadError, ExceptT, runExceptT, throwError)
import Control.Monad.State (MonadState, StateT, evalStateT, get, put, gets, modify)
import Control.Monad.Identity (runIdentity)

region :: MonadState s m => m a -> m a
region action = do
  state <- get
  result <- action
  put state
  return result

newtype CheckT m a =
  CheckT (ExceptT String (StateT Locals (ContextT m)) a)
  deriving (Functor, Applicative, Monad, MonadError String, MonadState Locals)

runCheckT :: Monad m => CheckT m a -> ContextT m (Either String a)
runCheckT (CheckT action) = evalStateT (runExceptT action) Locals.empty

instance Monad m => MonadContext (CheckT m) where
  access action = CheckT (lift $ lift $ access action)
  fresh = CheckT (lift $ lift fresh)
  reduce term = CheckT (lift $ lift $ reduce term)

instance Monad m => MonadEqual (CheckT m) where
  equal one other = CheckT (lift $ lift $ runEqualT $ equal one other)

bind :: Monad m => Type -> CheckT m String
bind tipe = do name <- fresh; modify (Locals.bind name tipe); return name

constrain :: Monad m => Variable -> Term -> CheckT m ()
constrain variable term = modify (Locals.constrain variable term)

infer :: Monad m => Term -> CheckT m Type
infer = \case
  Term.Global name -> access (Globals.declaration name) >>= \case
    Nothing -> throwError ("unknown global `" ++ name ++ "`")
    Just tipe -> return tipe

  Term.Local variable -> gets (Locals.bound variable) >>= \case
    Nothing -> error "unknown local variable -- should not happen"
    Just tipe -> return tipe

  Term.Type -> return Term.Type

  Term.FunctionType input scope -> region $ do
    check Term.Type input
    
    name <- bind input
    check Term.Type (open name scope)

    return Term.Type

  Term.Function _ -> throwError "functions don't have an inferable type"

  Term.Apply function argument -> infer function >>= reduce >>= \case
    Term.FunctionType input scope -> do
      check input argument 
      return (instantiate argument scope)
    
    _ -> throwError "function application type mismatch"

  Term.PairType input scope -> region $ do
    check Term.Type input 

    name <- bind input
    check Term.Type (open name scope) 

    return Term.Type

  Term.Pair _ _ -> throwError "pairs don't have an inferable type"

  Term.Split _ _ -> throwError "split expressions don't have an inferable type"

  Term.LabelType set -> do
    unless (unique set) (throwError "label type has repeated labels")
    return Term.Type

  Term.Label _ -> throwError "labels don't have an inferable type"

  Term.Match _ _ -> throwError "match expressions don't have an inferable type"

check :: Monad m => Type -> Term -> CheckT m ()
check tipe = \case
  Term.Function body -> reduce tipe >>= \case
    Term.FunctionType input scope -> region $ do
      name <- bind input
      check (open name scope) (open name body)
    
    _ -> throwError "function type mismatch"
  
  Term.Pair left right -> reduce tipe >>= \case
    Term.PairType input scope -> do
      check input left
      check (instantiate left scope) right
    
    _ -> throwError "pair type mismatch"
  
  Term.Split scrutinee body -> infer scrutinee >>= reduce >>= \case
    Term.PairType input scope -> region $ do
      left <- bind input
      right <- bind (open left scope)

      reduce scrutinee >>= \case
        Term.Local variable -> constrain variable pair where
          leftComponent = Term.Local (Variable left)
          rightComponent = Term.Local (Variable right)
          pair = Term.Pair leftComponent rightComponent

        _ -> return ()
      
      check tipe (open right (open left body))
      
    _ -> throwError "split expression scrutinee type mismatch"
  
  Term.Label label -> reduce tipe >>= \case
    Term.LabelType labels ->
      unless (elem label labels) (throwError "label does not belong to label type")
    
    _ -> throwError "label type mismatch"
  
  Term.Match scrutinee branches -> do
    let labels = map fst branches

    unless (unique labels)
      (throwError "match expression has repeated branch labels")
    
    infer scrutinee >>= reduce >>= \case
      Term.LabelType set -> do
        unless (labels <==> set)
          (throwError "match expression branch labels do not match scrutinee label type")
        
        go <- reduce scrutinee >>= return . \case
          Term.Local variable -> \(label, body) -> region $ do
            constrain variable (Term.Label label)
            check tipe body
          
          _ -> \(_, body) -> check tipe body
        
        mapM_ go branches

      _ -> throwError "match expression scrutinee type mismatch"

  term -> do
    tipe' <- infer term
    areEqual <- equal tipe tipe'
    unless areEqual (throwError "type mismatch")

checks :: Globals -> Type -> Term -> Either String ()
checks globals tipe term =
  runIdentity (runContextT (runCheckT (check tipe term)) globals)
