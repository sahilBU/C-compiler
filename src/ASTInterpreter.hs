module ASTInterpreter where
import CParser
import Ast
import StatefulUnsafeMonad
import ParserMonad
import TestsProject
-- import Prelude hiding (lookup)
import Data.Map(Map, lookup, insert, empty, fromList, findWithDefault)
-- Monad has three states :
--                   Global            Local           Output
type State = ((Map String Stmts),(Map String Integer),[String]) -- TODO change to be the type of state, you have freedom for how you implement it
-- return type of eval statement
data StmtRes = Normal | Ret Integer | RetBR | RetCN deriving(Show)

-- *****************************************
-- helper functions
-- change data type from int to [String]
intToSingleString :: Integer -> String
intToSingleString i = show i

intToString :: Integer -> [String]
intToString i = [show i]
-- get value from Monad, if nothing return -1
getValFromMaybe :: Maybe Integer -> Integer
getValFromMaybe (Just i) = i
getValFromMaybe nothing = -1
-- set value to state 
setVal :: String -> Integer -> StatefulUnsafe State StmtRes
setVal var i = StatefulUnsafe $ \(global_state, local_state, output) -> (Ok Normal, (global_state, insert var i local_state, output))
-- change data type from int to bool
intToBool :: Integer -> Bool
intToBool i = if (i <= 0) then False else True
-- get statement from global environment
getSt :: String -> Map String Stmts -> Stmts
getSt s m = case Data.Map.lookup s m of
                 (Just (FuncNoArg _ st)) -> st
                 (Just (Func _ _ st2)) -> st2
                 _ -> undefined

-- this method must be used in statements which have a integer as return value
getRetFromSt :: Stmts -> State -> (Unsafe Integer, State)
getRetFromSt stmts (global_state, local_state, output) = case ((app (evalSt stmts)) (global_state, local_state, output)) of
                                                              (Ok (Ret i), (global_state, _, newOutput)) -> (Ok i, (global_state, local_state, newOutput))
                                                              (_, (global_state, _, newOutput)) -> (Error "cant get value from statements which have no integer return value", (global_state, local_state, newOutput))
getRetFromStWithArgs :: Stmts -> State -> String -> [Expr] -> (Unsafe Integer, State)
getRetFromStWithArgs stmts (global_state, local_state, output) s exprs = case (app (evalSt stmts) (global_state, (insertArg (evalExs exprs (global_state, local_state, output)) (getStringFromExpr (getArgsFromSt s global_state)) local_state), output)) of 
                                                                              (Ok (Ret i), (global_state, _, newOutput)) -> (Ok i, (global_state, local_state, newOutput))
                                                                              (_, (global_state, _, newOutput)) -> (Error "something wrong with functions with arguments", (global_state, local_state, newOutput))
getArgsFromSt :: String -> Map String Stmts -> [Expr]
getArgsFromSt s m = case Data.Map.lookup s m of
                         (Just (Func _ (Arg li) _)) -> li
                         (Just (Func _ res _)) -> [res]
                         _ -> undefined
getStringFromExpr :: [Expr] -> [String]
getStringFromExpr [] = []
getStringFromExpr (e:ex) = case e of 
                                Var s -> [s] ++ getStringFromExpr ex
evalExs :: [Expr] -> State -> [Integer]
evalExs [] s = []
evalExs (e:es) s = case app (evalEx e) s of
                        (Ok res,_) -> [res] ++ evalExs es s
                        _ -> undefined
insertArg :: [Integer] -> [String] -> Map String Integer -> Map String Integer
insertArg [] [] map = map
insertArg (i:is) (s:sx) map = insert s i (insertArg is sx map)
-- change parse result to program
changeToProgram :: Maybe (a,String) -> a
changeToProgram (Just (x,_)) = x
changeToProgram _ = undefined
exprToList :: Expr -> [Expr]
exprToList e = [e]
-- evaluate a statement in a local environment
-- evalInState :: StatefulUnsafe State StmtRes -> State -> (Unsafe StmtRes, [String])
-- evalInState stateful state = case stateful state of
--                                   (Ok a, (_,_,output)) -> (Ok a, output)
--                                   _ ->  (Error "cant eval in state", output)
-- findArgList :: String -> Map String Stmts -> 
-- ******************************************
-- TODO : eval program
-- test : 
       --    1   Assign String Expr       
       --    1   While Expr Stmts |
       --    1   Block [Stmts] |
       --    1   If Expr Stmts |
       --    1   IfElse Expr Stmts Stmts |
       --    1   Func String Expr Stmts |
       --    1   FuncNoArg String Stmts |
       --    1   Return Expr |
       --    1   Print String |
       --    1   Break |
       --    1   Continue 
testCase = [FuncNoArg "main" (Block [Assign "x" (Val 1), Print "x", Assign "Y" (Call "test" (Arg [(Val 10),(Val 20)])), Print "Y"]),Func "test" (Arg [Var "a", Var "b"])(Block [Return (Plus (Var "a") (Var "b"))])]
-- While (Lt (Var "x") (Val 5) ) (Block [Print "x", Assign "x" (Plus (Var "x") (Val 1))])
-- testCase = Func "main" (Arg [(Val 1), (Val 2)]) (Print "x")
-- test = app (evalSt testCase) (Data.Map.empty, insert "x" 0 Data.Map.empty, [])
evalSt :: Stmts -> StatefulUnsafe State StmtRes
evalSt (Assign s e) = do res <- evalEx e
                         setVal s res
evalSt (While e stmt) = do res <- evalEx e
                           if res <= 0 then return Normal else 
                              do res2 <- evalSt stmt
                                 case res2 of
                                      Ret i -> return (Ret i)
                                      RetBR -> return Normal
                                      -- includes continue and normal
                                      _ -> evalSt (While e stmt) 
evalSt (Block []) = return Normal
evalSt (Block (x:xs)) = do res <- evalSt x
                           case res of
                                Normal -> evalSt (Block xs)
                                Ret i -> return (Ret i)
                                RetBR -> return (RetBR)
                                RetCN -> return (RetCN)
evalSt (Print s) = StatefulUnsafe $ \(global_state, local_state, output) -> (Ok Normal, (global_state, local_state, output ++ [((concat[s, "=", intToSingleString (getValFromMaybe(Data.Map.lookup s local_state))]))]))
evalSt (If cond stmt) = do res <- evalEx cond
                           if (res > 0) then evalSt stmt else return Normal 
evalSt (IfElse cond stmt1 stmt2) = do res <- evalEx cond
                                      if (res > 0) then evalSt stmt1 else evalSt stmt2
evalSt (Break) = return RetBR
evalSt (Continue) = return RetCN
evalSt (Return e) = do res <- evalEx e
                       return (Ret res)
-- Assume we do not have independent function calls
evalSt (FuncNoArg name stmt) = StatefulUnsafe $ \(global_state, local_state, output) -> (Ok Normal, (insert name (FuncNoArg name stmt) global_state, local_state, output))
evalSt (Func name args stmt) = StatefulUnsafe $ \(global_state, local_state, output) -> (Ok Normal, (insert name (Func name args stmt) global_state, local_state, output))
evalSt _ = undefined

evalEx :: Expr -> StatefulUnsafe State Integer
evalEx (Val num) = return num
evalEx (Var s) = StatefulUnsafe $ \(global_state, local_state, output) -> let x = Data.Map.lookup s local_state
                                                                          in ((Ok (getValFromMaybe x)), (global_state, local_state, output))
evalEx (Plus a b) = do left <- evalEx a
                       right <- evalEx b
                       return (left + right)
evalEx (Minus l r) = do left <- evalEx l
                        right <- evalEx r
                        return (left - right)
evalEx (Times l r) = do left <- evalEx l
                        right <- evalEx r
                        return (left * right)
evalEx (Div l r) = do left <- evalEx l
                      right <- evalEx r
                      case right of
                           0 -> err "divid by 0"
                           _ -> return (left `div` right)
evalEx (Mod l r) = do left <- evalEx l
                      right <- evalEx r
                      case right of
                           0 -> err "Mod by 0"
                           _ -> return (left `mod` right)
evalEx (And l r) = do left <- evalEx l
                      case (intToBool left) of
                            False -> return 0
                            True -> (do right <- evalEx r
                                        if ((intToBool left) && (intToBool right)) then (return 1) else (return 0))
evalEx (Or l r) = do left <- evalEx l
                     case (intToBool left) of
                           True -> return 1
                           False -> (do right <- evalEx r
                                        if ((intToBool left) || (intToBool right)) then return 1 else return 0)
evalEx (Not r) = do right <- evalEx r
                    if (not (intToBool right)) then return 1 else return 0
evalEx (Eq l r) = do left <- evalEx l
                     right <- evalEx r
                     if left == right then return 1 else return 0
evalEx (NotEq l r) = do left <- evalEx l
                        right <- evalEx r
                        if left /= right then return 1 else return 0
evalEx (Lt l r) = do left <- evalEx l
                     right <- evalEx r
                     if left < right then return 1 else return 0
evalEx (Le l r) = do left <- evalEx l
                     right <- evalEx r
                     if left <= right then return 1 else return 0
evalEx (Gt l r) = do left <- evalEx l
                     right <- evalEx r
                     if left > right then return 1 else return 0
evalEx (Ge l r) = do left <- evalEx l
                     right <- evalEx r
                     if left >= right then return 1 else return 0
evalEx (Rev v) = do v <- evalEx v
                    return (negate v)
-- TODO:eval functions includes arguments
-- evalEx (Call s Arg[x:xs]) =   
-- each time reset local environment to empty
evalEx (CallNoArg s) = StatefulUnsafe $ \(global_state, local_state, output) -> (getRetFromSt (getSt s global_state) (global_state, local_state, output))
evalEx (Call s exps) = StatefulUnsafe $ \ (global_state, local_state, output) -> (getRetFromStWithArgs (getSt s global_state) (global_state, local_state, output) s (exprToList exps))
evalEx (Call s (Arg exps)) = StatefulUnsafe $ \ (global_state, local_state, output) -> (getRetFromStWithArgs (getSt s global_state) (global_state, local_state, output) s exps)
evalEx _ = undefined
-- cleanLocals :: StatefulUnsafe State a -> StatefulUnsafe State a
-- cleanLocals x = StatefulUnsafe $ \ (global_state, local_state, output) -> case (app x (global_state, Data.Map.empty, output)) of
                                                                               -- (Ok a, (_, _, newOutput)) -> (Ok a,(global_state, local_state, newOutput))
-- order : 
-- 1. eval all the functions
-- 2. eval main function
-- 3. get output from state monad
eval :: Program -> [String]
eval p = evalMain (evalFuncs p (Data.Map.empty, Data.Map.empty, []))
-- get global environment
evalFuncs :: Program -> State -> State
evalFuncs [] st = st
evalFuncs (x:xs) st =  case app (evalSt x) st of
                            (_,newState) -> evalFuncs xs newState
evalMain :: State -> [String]
evalMain (global_state, local_state, output) = case app (evalSt (getSt "main" global_state)) (global_state, local_state, output) of
                                                    (_, (_,_,outputSt)) -> outputSt

test :: String ->[String]
test s = eval (changeToProgram (parse parser s))
-- test : 1   5. 6. 7. 8. 9 10 11 12                                                
-- eval p = getPrints $ snd $ app (eval' p) undefined
--
-- getPrints :: State -> [String]
-- getPrints = undefined
--
-- eval' :: Program -> StatefulUnsafe State Int
-- eval' = undefined
