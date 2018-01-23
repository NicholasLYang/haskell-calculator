import Lexer
import Parser
import Evaluator

main = (print . evaluate . parse . tokenize) "x1 = -15 - 3"
