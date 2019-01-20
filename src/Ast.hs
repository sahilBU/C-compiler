module Ast where

type Program = [Stmts]
type Arguments = [Expr] -- TODO: your ast here

-- TODO: Ast should have at least Eq and Show instances

data Stmts = Assign String Expr |
             While Expr Stmts |
             Block [Stmts] |
             If Expr Stmts |
             IfElse Expr Stmts Stmts |
			 Func String Expr Stmts |
             FuncNoArg String Stmts |
             Return Expr |
             Print String|
             Break |
             Continue 
             deriving Eq
--  TODO : Arg [Expr] eval?
data Expr = Val Integer | 
            Var String |
            Plus Expr Expr | 
            Minus Expr Expr | 
            Times Expr Expr | 
            Div Expr Expr |
            Mod Expr Expr |
            And Expr Expr |
            Or Expr Expr |
            Not Expr |
            Arg [Expr] |
            Eq Expr Expr |
            NotEq Expr Expr |
            Lt Expr Expr |
            Gt Expr Expr |
            Le Expr Expr |
            Ge Expr Expr |
            Rev Expr |
			Call String Expr |
			CallNoArg String
            deriving Eq
			
instance Show Expr where
  show a = prettyShowE a 
  
instance Show Stmts where
  show a = prettyShowS a
--eval :: Expr -> Integer
--eval (Val x) = x
--eval (Plus x y) = eval x + eval y 

--instance Show Expr where
  --show (Val x) = show x
  --show (Plus x y) = show (eval x) ++ "+" ++ show (eval y)


prettyShowS :: Stmts -> String
prettyShowS (Assign s a) = s ++ " = " ++ (prettyShowE a)
prettyShowS (While a s) = "While " ++ (prettyShowE a) ++ " " ++ (prettyShowS s)
prettyShowS (Block []) = ""
prettyShowS (Block (s:rest)) = (prettyShowS s) ++ " " ++ (prettyShowS (Block rest))
prettyShowS (If x y) = "if " ++ (prettyShowE x) ++ " " ++ (prettyShowS y)
prettyShowS (IfElse x y z) = "if " ++ (prettyShowE x) ++ " " ++ (prettyShowS y) ++ " else " ++ (prettyShowS z)
prettyShowS (Func x y z) = "def " ++ x ++ " " ++ (prettyShowE y) ++ " " ++ (prettyShowS z)
prettyShowS (FuncNoArg x y) = "def " ++ x ++ " " ++ (prettyShowS y)
prettyShowS (Return x) = "return " ++ (prettyShowE x)
prettyShowS (Print x) = "print " ++ x
prettyShowS (Break) = "break"
prettyShowS (Continue) = "continue"
 
prettyShowE :: Expr -> String
prettyShowE (Val i) = if i < 0
                      then  "(" ++ show i ++ ")"
                      else show i
prettyShowE (Plus x y) = (prettyShowE x) ++ " + " ++ (prettyShowE y)
prettyShowE (Minus x y) = (prettyShowE x) ++ " - " ++ (prettyShowE y)
prettyShowE (Times x y) = (prettyShowE x) ++ " * " ++ (prettyShowE y)
prettyShowE (Div x y) = (prettyShowE x) ++ " / " ++ (prettyShowE y)
prettyShowE (Mod x y) = (prettyShowE x) ++ " % " ++ (prettyShowE y)
prettyShowE (And x y) = (prettyShowE x) ++ " && " ++ (prettyShowE y)
prettyShowE (Or x y) = (prettyShowE x) ++ " || " ++ (prettyShowE y)
prettyShowE (Not x) = " ! " ++ (prettyShowE x)
prettyShowE (Var x) = x
prettyShowE (Arg []) = ""
prettyShowE (Arg (s:rest)) = (prettyShowE s) ++ " " ++ (prettyShowE (Arg rest))
prettyShowE (Eq x y) = (prettyShowE x) ++ " == " ++ (prettyShowE y)
prettyShowE (NotEq x y) = (prettyShowE x) ++ " /= " ++ (prettyShowE y)
prettyShowE (Lt x y) = (prettyShowE x) ++ " < " ++ (prettyShowE y)
prettyShowE (Gt x y) = (prettyShowE x) ++ " > " ++ (prettyShowE y)
prettyShowE (Le x y) = (prettyShowE x) ++ " <= " ++ (prettyShowE y)
prettyShowE (Ge x y) = (prettyShowE x) ++ " >= " ++ (prettyShowE y)
prettyShowE (Rev x) = " -" ++ (prettyShowE x)
prettyShowE (Call x y) = x ++ "(" ++ (prettyShowE y) ++ ")"
prettyShowE (CallNoArg x) = x ++ "()"