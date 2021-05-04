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
import Control.Monad.Identity (runIdentity)

newtype CheckT m a =
  CheckT (ExceptT String (ContextT m) a)
  deriving (Functor, Applicative, Monad, MonadError String)

runCheckT :: Monad m => CheckT m a -> ContextT m (Either String a)
runCheckT (CheckT action) = runExceptT action

instance Monad m => MonadContext (CheckT m) where
  access action = CheckT (lift $ access action)
  fresh = CheckT (lift fresh)
  reduce term = CheckT (lift $ reduce term)

instance Monad m => MonadEqual (CheckT m) where
  equal one other = CheckT (lift $ runEqualT $ equal one other)

infer :: Monad m => Locals -> Term -> CheckT m Type
infer locals = \case
  Term.Global name -> access (Globals.declaration name) >>= \case
    Nothing -> throwError ("unknown global `" ++ name ++ "`")
    Just tipe -> return tipe

  Term.Local variable -> case Locals.bound variable locals of
    Nothing -> error "unknown local variable -- should not happen"
    Just tipe -> return tipe

  Term.Type -> return Term.Type

  Term.FunctionType input scope -> do
    check locals Term.Type input 

    name <- fresh
    check (Locals.bind name input locals) Term.Type (open name scope)

    return Term.Type

  Term.Function _ -> throwError "functions don't have an inferable type"

  Term.Apply function argument -> infer locals function >>= reduce >>= \case
    Term.FunctionType input scope -> do
      check locals input argument 
      return (instantiate argument scope)
    
    _ -> throwError "function application type mismatch"

  Term.PairType input scope -> do
    check locals Term.Type input 

    name <- fresh
    check (Locals.bind name input locals) Term.Type (open name scope) 

    return Term.Type

  Term.Pair _ _ -> throwError "pairs don't have an inferable type"

  Term.Split _ _ -> throwError "split expressions don't have an inferable type"

  Term.LabelType set -> do
    unless (unique set) (throwError "label type has repeated labels")
    return Term.Type

  Term.Label _ -> throwError "labels don't have an inferable type"

  Term.Match _ _ -> throwError "match expressions don't have an inferable type"

check :: Monad m => Locals -> Type -> Term -> CheckT m ()
check locals tipe = \case
  Term.Function body -> reduce tipe >>= \case
    Term.FunctionType input scope -> do
      name <- fresh
      check (Locals.bind name input locals) (open name scope) (open name body)
    
    _ -> throwError "function type mismatch"
  
  Term.Pair left right -> reduce tipe >>= \case
    Term.PairType input scope -> do
      check locals input left
      check locals (instantiate left scope) right
    
    _ -> throwError "pair type mismatch"
  
  Term.Split scrutinee body -> infer locals scrutinee >>= reduce >>= \case
    Term.PairType input scope -> do
      left <- fresh
      right <- fresh

      bound <- return $ Locals.bind right (open left scope) 
        $ Locals.bind left input
        $ locals

      constrained <- reduce scrutinee >>= return . \case
        Term.Local variable -> Locals.constrain variable pair bound where
          leftComponent = Term.Local (Variable left)
          rightComponent = Term.Local (Variable right)
          pair = Term.Pair leftComponent rightComponent

        _ -> bound
      
      check constrained tipe (open right (open left body))

    _ -> throwError "split expression scrutinee type mismatch"
  
  Term.Label label -> reduce tipe >>= \case
    Term.LabelType labels ->
      unless (elem label labels) (throwError "label does not belong to label type")
    
    _ -> throwError "label type mismatch"
  
  Term.Match scrutinee branches -> do
    labels <- return $ map fst branches

    unless (unique labels)
      (throwError "match expression has repeated branch labels")
    
    infer locals scrutinee >>= reduce >>= \case
      Term.LabelType set -> do
        unless (labels <==> set)
          (throwError "match expression branch labels do not match scrutinee label type")
        
        go <- reduce scrutinee >>= return . \case
          Term.Local variable -> \(label, body) ->
            check (Locals.constrain variable (Term.Label label) locals) tipe body
          
          _ -> \(_, body) -> check locals tipe body
        
        mapM_ go branches

      _ -> throwError "match expression scrutinee type mismatch"

  term -> do
    tipe' <- infer locals term
    areEqual <- equal tipe tipe'
    unless areEqual (throwError "type mismatch")

checks :: Globals -> Type -> Term -> Either String ()
checks globals tipe term =
  runIdentity (runContextT (runCheckT (check Locals.empty tipe term)) globals)
