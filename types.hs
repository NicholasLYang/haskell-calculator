module Types(Operator(..), Token(..), Tree(..))
  where
data Operator = Plus | Minus | Times | Div
     deriving (Show, Eq)

data Token = TokOp Operator
           | TokIdent String
           | TokNum Double
           | TokSpace
           | TokAssign
           | TokLParen
           | TokRParen
           | TokEnd
    deriving (Show, Eq)

data Tree = SumNode Operator Tree Tree
          | ProdNode Operator Tree Tree
          | AssignNode String Tree
          | UnaryNode Operator Tree
          | NumNode Double
          | VarNode String
    deriving Show
