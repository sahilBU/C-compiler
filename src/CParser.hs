module CParser where

import Ast
import ParserMonad

keywords = ["if","then","else", "let", "in", "true","false","def"]

parser :: Parser Program
parser = (do s <- token $ parserS
             rest <- token $ parser
             return (s:rest))
             <||> do s <- token $ parserS
                     return [s]
					 
parserA :: Parser Arguments
parserA = (do s <- parserE
              rest <- parserA
              return (s:rest))
              <||> do s <- parserE
                      return [s]

parserE :: Parser Expr
parserE = orParser <||> andParser <||> notEqParser <||> compareEqParser <||> compareParser <||> addSubExpr <||> multDivExpr <||> revParser <||> notExp <||> atoms

parserS :: Parser Stmts
parserS = funcParser <||> funcNoArgParser <||> whileParser <||> assignParser <||> ifElseParser <||> ifParser <||> returnParser <||> printParser <||> continueParser <||> breakParser


ints :: Parser Expr
ints = do i <- token $ intParser
          return $ Val i
		  
vars :: Parser Expr
vars = do s <- token $ varParser
          if s `elem` keywords
          then failParse
          else return $ Var s
		  
addSubExpr :: Parser Expr
addSubExpr = withInfix multDivExpr [("+",Plus),("-", Minus)]

multDivExpr :: Parser Expr
multDivExpr = withInfix revParser [("*",Times),("/",Div),("%",Mod)]

orParser :: Parser Expr
orParser = withInfix andParser [("||", Or)]

andParser :: Parser Expr
andParser = withInfix notEqParser [("&&", And)]

notExp :: Parser Expr
notExp = (do token $ literal "!"
             ares <- notExp
             return $ Not ares)
			 <||> atoms
               			   
notEqParser :: Parser Expr
notEqParser = withInfix compareEqParser [("==",Eq),("!=",NotEq)]

compareEqParser :: Parser Expr
compareEqParser = withInfix compareParser [("<=",Le),(">=",Ge)]

compareParser :: Parser Expr
compareParser = withInfix addSubExpr [("<",Lt),(">",Gt)]
                  			              		 
argParser :: Parser Expr
argParser = (do s <- token $ parserE
                rest <- token $ parserA
                return $ (Arg (s:rest)))
                <||> do s <- token $ parserE
                        return $ (Arg [s])
						
revParser :: Parser Expr
revParser = (do token $ literal "-"
                x <- token $ revParser
                return $ Rev x)
                <||> notExp
				

atoms :: Parser Expr
atoms = ints <||> callParser <||> callNoArgParser <||> parens <||> vars

parens :: Parser Expr
parens = do token (literal "(")
            res <- parserE
            token (literal ")")
            return res
			
parensStmts :: Parser Stmts
parensStmts = do token (literal "(")
                 res <- parserS
                 token (literal ")")
                 return res
			
callParser :: Parser Expr
callParser = do x <- token $ varParser
                token $ literal "("
                y <- token $ parserE
                token $ literal ")"
                return $ Call x y
				
callNoArgParser :: Parser Expr
callNoArgParser = do x <- token $ varParser
                     token $ literal "("
                     token $ literal ")"
                     return $ CallNoArg x
			
assignParser :: Parser Stmts
assignParser = do l <- token $ varParser
                  token $ literal "="
                  r <- token $ parserE
                  token $ literal ";"
                  return $ Assign l r
whileParser :: Parser Stmts
whileParser = do token $ literal "while"
                 token $ literal "("
                 l <- token $ parserE
                 token $ literal ")"
                 token $ literal "{"
                 r <- token $ blockParser
                 token $ literal "}"
                 return $ While l r	
				 
blockParser :: Parser Stmts
blockParser = (do s <- token $ parserS
                  rest <- token $ parser
                  return $ (Block (s:rest)))
                  <||> do s <- token $ parserS
                          return $ (Block [s])
						  
ifParser :: Parser Stmts
ifParser = do token $ literal "if"
              token $ literal "("
              l <- token $ parserE
              token $ literal ")"
              token $ literal "{"
              r <- token $ blockParser
              token $ literal "}"
              return $ If l r

ifElseParser :: Parser Stmts
ifElseParser = do token $ literal "if"
                  token $ literal "("
                  x <- token $ parserE
                  token $ literal ")"
                  token $ literal "{"
                  y <- token $ blockParser
                  token $ literal "}"
                  token $ literal "else"
                  token $ literal "{"
                  z <- token $ blockParser
                  token $ literal "}"
                  return $ IfElse x y z
				  
funcParser :: Parser Stmts
funcParser = do token $ literal "def"
                x <- token $ varParser
                token $ literal "("
                y <- token $ parserE
                token $ literal ")"
                token $ literal "{"
                z <- token $ blockParser
                token $ literal "}"
                return $ Func x y z

funcNoArgParser :: Parser Stmts
funcNoArgParser = do token $ literal "def"
                     x <- token $ varParser
                     token $ literal "() {"
                     y <- token $ blockParser
                     token $ literal "}"
                     return $ FuncNoArg x y				

				
returnParser :: Parser Stmts
returnParser = do token $ literal "return"
                  x <- token $ parserE
                  token $ literal ";"
                  return $ Return x
				  
printParser :: Parser Stmts
printParser = do token $ literal "print"
                 x <- token $ varParser
                 token $ literal ";"
                 return $ Print x
				 
breakParser :: Parser Stmts
breakParser = do token $ literal "break;"
                 return $ Break
				 
continueParser :: Parser Stmts
continueParser = do token $ literal "continue;"
                    return $ Continue
				 

   



