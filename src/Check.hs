{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Check
  ( checks
  )
  where

import Term (Variable (Variable), unwrap, Type, Term, instantiate, open)
import qualified Term as Term

import Context (Context)
import qualified Context as Context

import Util (unique, (<==>))
import Base (MonadBase, BaseT, runBaseT, askGlobals, getLocals, putLocals, fresh, reduce)
import Equal (MonadEqual, runEqualT, equal)

import Control.Monad (unless)
import Control.Monad.Trans (lift)
import Control.Monad.Except (MonadError, ExceptT, runExceptT, throwError)
import Control.Monad.Identity (runIdentity)

newtype CheckT m a =
  CheckT (ExceptT String (BaseT m) a)
  deriving (Functor, Applicative, Monad, MonadError String)

runCheckT :: Monad m => CheckT m a -> BaseT m (Either String a)
runCheckT (CheckT action) = runExceptT action

instance Monad m => MonadBase (CheckT m) where
  askGlobals = CheckT (lift askGlobals)
  getLocals = CheckT (lift getLocals)
  putLocals locals = CheckT (lift $ putLocals locals)
  fresh = CheckT (lift fresh)
  reduce term = CheckT (lift $ reduce term)

instance Monad m => MonadEqual (CheckT m) where
  equal one other = CheckT (lift $ runEqualT $ equal one other)

region :: Monad m => CheckT m a -> CheckT m a
region action = do
  locals <- getLocals
  result <- action
  putLocals locals
  return result

bind :: Monad m => Type -> CheckT m String
bind tipe = do
  name <- fresh
  locals <- getLocals
  putLocals (Context.declare name tipe locals)
  return name

constrain :: Monad m => String -> Term -> CheckT m ()
constrain name term = do
  locals <- getLocals
  putLocals (Context.define name term locals)

infer :: Monad m => Term -> CheckT m Type
infer = \case
  Term.Global name -> do
    globals <- askGlobals

    case Context.declaration name globals of
      Nothing -> throwError ("unknown global `" ++ name ++ "`")
      Just tipe -> return tipe

  Term.Local variable -> do
    locals <- getLocals

    case Context.declaration (unwrap variable) locals of
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
        Term.Local variable -> constrain (unwrap variable) pair where
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
            constrain (unwrap variable) (Term.Label label)
            check tipe body
          
          _ -> \(_, body) -> check tipe body
        
        mapM_ go branches

      _ -> throwError "match expression scrutinee type mismatch"

  term -> do
    tipe' <- infer term
    areEqual <- equal tipe tipe'
    unless areEqual (throwError "type mismatch")

checks :: Context -> Type -> Term -> Either String ()
checks globals tipe term =
  runIdentity (runBaseT (runCheckT (check tipe term)) globals)
