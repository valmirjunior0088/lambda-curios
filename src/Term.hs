module Term
  ( Variable (Variable)
  , unwrap
  , Scope
  , Type
  , Term (..)
  , Walk
  , abstract
  , instantiate
  , open
  )
  where

import Control.Monad.Reader (Reader, runReader, ask, local)

data Variable =
  Variable String |
  Bound Int
  deriving (Show, Eq)

unwrap :: Variable -> String
unwrap = \case
  Variable name -> name
  Bound _ -> error "bound variable -- should not happen"

newtype Scope a = Scope a

instance Show a => Show (Scope a) where
  show (Scope scope) = "{" ++ show scope ++ "}"

instance Eq a => Eq (Scope a) where
  (==) (Scope one) (Scope other) = one == other

type Type = Term

data Term =
  Global String |
  Local Variable |

  Type |

  FunctionType Type (Scope Type) |
  Function (Scope Term) |
  Apply Term Term |

  PairType Type (Scope Type) |
  Pair Term Term |
  Split Term (Scope (Scope Term)) |
  
  LabelType [String] |
  Label String |
  Match Term [(String, Term)]
  deriving (Show, Eq)

type Depth = Reader Int

class Walk a where
  walk :: (Variable -> Depth Term) -> a -> Depth a

instance Walk Term where
  walk action = \case 
    Global name -> Global <$> pure name
    Local variable -> action variable
    Type -> Type <$ pure ()
    FunctionType input scope -> FunctionType <$> walk action input <*> walk action scope
    Function body -> Function <$> walk action body
    Apply function argument -> Apply <$> walk action function <*> walk action argument
    PairType input scope -> PairType <$> walk action input <*> walk action scope
    Pair left right -> Pair <$> walk action left <*> walk action right
    Split scrutinee body -> Split <$> walk action scrutinee <*> walk action body
    LabelType set -> LabelType <$> pure set
    Label label -> Label <$> pure label
    Match scrutinee branches -> Match <$> walk action scrutinee <*> mapM (mapM (walk action)) branches

instance Walk a => Walk (Scope a) where
  walk action (Scope scope) = Scope <$> local succ (walk action scope)

with :: Walk a => (Int -> Variable -> Term) -> a -> a
with action subject = runReader (walk go subject) 0 where
  go :: Variable -> Depth Term
  go variable = do depth <- ask; pure (action depth variable)

abstract :: Walk a => String -> a -> Scope a
abstract target subject = Scope (with go subject) where
  go depth = \case
    Variable name | name == target -> Local (Bound depth)
    variable -> Local variable

instantiate :: Walk a => Term -> Scope a -> a
instantiate term (Scope subject) = with go subject where
  go depth = \case
    Bound index | index == depth -> term
    variable -> Local variable

open :: Walk a => String -> Scope a -> a
open name scope = instantiate (Local (Variable name)) scope
