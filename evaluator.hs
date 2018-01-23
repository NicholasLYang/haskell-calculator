module Evaluator(evaluate)
  where

import Types

evaluate :: Tree -> Double

evaluate (NumNode x) = x
evaluate (SumNode op left right) =
  let lft = evaluate left
      rgt = evaluate right
  in case op of
    Plus  -> lft + rgt
    Minus -> lft - rgt

evaluate (ProdNode op left right) =
  let lft = evaluate left
      rgt = evaluate right
     in case op of
        Times -> lft * rgt
        Div   -> lft / rgt
        
evaluate (UnaryNode op tree) =
  let x = evaluate tree
  in case op of
    Plus -> x
    Minus -> -x

-- Dummy implementation
evaluate (AssignNode str tree) = evaluate tree

evaluate (VarNode str) = 0

