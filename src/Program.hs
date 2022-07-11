module Program
  ( Entry (Declaration, Definition)
  , Program (Program)
  , run
  )
  where

import Term (Type, Term)
import qualified Term as Term

import Context (Context)
import qualified Context as Context

import Check (checks)

import Control.Monad (when)
import Data.Foldable (foldlM)

data Entry =
  Declaration String Type |
  Definition String Term
  deriving (Show)

newtype Program =
  Program [Entry]
  deriving (Show)

declare :: String -> Type -> Context -> Either String Context
declare name tipe context = do
  when (Context.declared name context)
    (Left "declared name being declared again")

  checks context Term.Type tipe
  Right (Context.declare name tipe context)

define :: String -> Term -> Context -> Either String Context
define name term context = do
  tipe <- case Context.declaration name context of
    Nothing -> Left "undeclared name being defined"
    Just tipe -> Right tipe
  
  checks context tipe term
  Right (Context.define name term context)

handle :: Context -> Entry -> Either String Context
handle context = \case
  Declaration name tipe -> declare name tipe context
  Definition name term -> define name term context

run :: Program -> Either String Context
run (Program entries) = foldlM handle Context.empty entries
