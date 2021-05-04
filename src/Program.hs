module Program
  ( Entry (Declaration, Definition)
  , Program (Program)
  , run
  )
  where

import Term (Type, Term)
import qualified Term as Term

import Globals (Globals)
import qualified Globals as Globals

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

declare :: String -> Type -> Globals -> Either String Globals
declare name tipe globals = do
  when (Globals.declared name globals)
    (Left "declared name being declared again")

  checks globals Term.Type tipe
  Right (Globals.declare name tipe globals)

define :: String -> Term -> Globals -> Either String Globals
define name term globals = do
  tipe <- case Globals.declaration name globals of
    Nothing -> Left "undeclared name being defined"
    Just tipe -> Right tipe
  
  checks globals tipe term
  Right (Globals.define name term globals)

handle :: Globals -> Entry -> Either String Globals
handle globals = \case
  Declaration name tipe -> declare name tipe globals
  Definition name term -> define name term globals

run :: Program -> Either String Globals
run (Program entries) = foldlM handle Globals.empty entries
