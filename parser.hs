module Parser(parse)
  where

import Data.Char
import Types

parse :: [Token] -> Tree
parse toks = let (tree, toks') = expression toks
             in
               if null toks'
               then tree
               else error $ "Leftover tokens: " ++ show toks'

expression :: [Token] -> (Tree, [Token])
expression toks =
  let (termTree, toks') = term toks
  in
    case lookAhead toks' of
      -- Term [+-] Expression
      (TokOp op) | elem op [Plus, Minus] ->
        let (exTree, toks'') = expression (accept toks')
        in (SumNode op termTree exTree, toks'')
      -- Identifier '=' Expression
      TokAssign ->
        case termTree of
          VarNode str ->
            let (exTree, toks'') = expression (accept toks')
            in (AssignNode str exTree, toks'')
          _ -> error "Only variables can be assigned to"
      -- Term
      _ -> (termTree, toks')

term       :: [Token] -> (Tree, [Token])
term toks =
  let (facTree, toks') = factor toks
  in
    case lookAhead toks' of
      (TokOp op) | elem op [Times, Div] ->
                   let (termTree, toks'') = term (accept toks')
                   in (ProdNode op facTree termTree, toks'')
      _ -> (facTree, toks')

factor     :: [Token] -> (Tree, [Token])
factor toks =
  case lookAhead toks of
    (TokNum x) -> (NumNode x, accept toks)
    (TokIdent str) -> (VarNode str, accept toks)
    (TokOp op) | elem op [Plus, Minus] ->
                 let (facTree, toks') = factor (accept toks)
                 in (UnaryNode op facTree, toks')
    TokLParen ->
      let (expTree, toks') = expression (accept toks)
      in
        if lookAhead toks' /= TokRParen
        then error "Missing right parenthesis"
        else (expTree, accept toks')
    _ -> error $ "Parse error on token: " ++ show toks

lookAhead :: [Token] -> Token
lookAhead [] = TokEnd
lookAhead (c:cs) = c

accept :: [Token] -> [Token]
accept [] = error "Nothing to accept"
accept (t:ts) = ts
