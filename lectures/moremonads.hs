data Exp = 
  Base Integer
  | Add Exp Exp
  | Mul Exp Exp
  | Div Exp Exp
  deriving (Show, Eq)

