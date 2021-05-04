module Globals
  ( Globals
  , empty
  , declared
  , declare
  , declaration
  , define
  , definition
  )
  where

import Term (Type, Term)

import Data.Map (Map)
import qualified Data.Map as Map

data Globals = Globals
  { declarations :: Map String Type
  , definitions :: Map String Term
  }
  deriving (Show)

empty :: Globals
empty = Globals
  { declarations = Map.empty
  , definitions = Map.empty
  }

declared :: String -> Globals -> Bool
declared name Globals { declarations } = Map.member name declarations

declare :: String -> Type -> Globals -> Globals
declare name tipe Globals { declarations, definitions } =
  Globals { declarations = Map.insert name tipe declarations, definitions }

declaration :: String -> Globals -> Maybe Type
declaration name Globals { declarations } = Map.lookup name declarations

define :: String -> Term -> Globals -> Globals
define name term Globals { declarations, definitions } =
  Globals { declarations, definitions = Map.insert name term definitions }

definition :: String -> Globals -> Maybe Term
definition name Globals { definitions } = Map.lookup name definitions
