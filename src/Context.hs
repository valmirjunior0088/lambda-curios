module Context
  ( Context
  , empty
  , declare
  , declaration
  , declared
  , define
  , definition
  )
  where

import Term (Type, Term)

import Data.Map (Map)
import qualified Data.Map as Map

data Context = Context
  { declarations :: Map String Type
  , definitions :: Map String Term
  }
  deriving (Show)

empty :: Context
empty = Context
  { declarations = Map.empty
  , definitions = Map.empty
  }

declare :: String -> Type -> Context -> Context
declare name tipe Context { declarations, definitions } =
  Context { declarations = Map.insert name tipe declarations, definitions }

declaration :: String -> Context -> Maybe Type
declaration name Context { declarations } = Map.lookup name declarations

declared :: String -> Context -> Bool
declared name Context { declarations } = Map.member name declarations

define :: String -> Term -> Context -> Context
define name term Context { declarations, definitions } =
  Context { declarations, definitions = Map.insert name term definitions }

definition :: String -> Context -> Maybe Term
definition name Context { definitions } = Map.lookup name definitions
