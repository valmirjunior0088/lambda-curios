module Locals
  ( Locals
  , empty
  , bind
  , constrain
  , bound
  )
  where

import Term (Variable, unwrap, Type, Term, abstract, instantiate)

import Data.Map (Map)
import qualified Data.Map as Map

substitute :: String -> Term -> Term -> Term
substitute target source term = instantiate source (abstract target term)

newtype Locals = Locals (Map String Type)

empty :: Locals
empty = Locals Map.empty

bind :: String -> Type -> Locals -> Locals
bind name tipe (Locals locals) = Locals (Map.insert name tipe locals)

constrain :: Variable -> Term -> Locals -> Locals
constrain variable source (Locals locals) =
  let go name = Locals (substitute name source <$> locals) in unwrap go variable

bound :: Variable -> Locals -> Maybe Type
bound variable (Locals locals) = let go name = Map.lookup name locals in unwrap go variable
