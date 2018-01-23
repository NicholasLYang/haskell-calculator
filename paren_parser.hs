data Token = TokLParen | TokRParen | TokEnd
  deriving (Show, Eq)

lookAhead :: [Char] -> Token
lookAhead [] = TokEnd
lookAhead (c:cs)| c == '(' = TokLParen
                | c == ')' = TokRParen
                | otherwise = error $ "Bad input: " ++ (c:cs)

accept :: [Char] -> [Char]
accept [] = error "Nothing to accept"
accept (t:ts) = ts

data Tree = Node Tree Tree | Leaf
  deriving Show

root, expr, par :: [Char] -> (Tree, [Char])
root = par

expr toks =
  let (parTree, toks') = par toks
      (parTree', toks'') = par toks'
  in (Node parTree parTree', toks'')


par toks =
  case lookAhead toks of
    TokLParen ->
      case lookAhead (accept toks) of
        TokRParen -> (Leaf, accept (accept toks))
        _ -> let (exprTree, toks') = expr (accept toks)
          in
            if lookAhead toks' == TokRParen
            then (exprTree, accept toks')
            else error $ "Missing right parenthesis at: " ++ show toks'
    _ -> error $ "Parse error on token: " ++ show toks

parse :: [Char] -> Tree
parse toks = let (tree, toks') = root toks
            in
              if null toks'
              then tree
              else error $ "Leftover tokens: " ++ toks'

main = print $ parse "(()(()()))"
