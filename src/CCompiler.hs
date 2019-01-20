module CCompiler where

import Ast
import CParser
import ASTInterpreter
import ICInterpreter
import TestsProject
import ParserMonad
import Data.Map(Map, lookup, insert, empty, fromList, findWithDefault)
import StatefulUnsafeMonad

-- map for functions, @param function_name, function_arguments
type Symbol_table = Map String (Integer, Expr, Stmts)
-- initialized temp ts
temps = ["_t" ++ show n | n <- [1..]]
testC = ([Push',Call' 3,Halt'],temps,([],[],[],[]),Data.Map.empty)
-- backpatching list, True, False, Break, Continue
type BP = ([Int], [Int], [Int], [Int])
-- state for monad, IC_program, temps, symbol_table
type Compile_State = (IC_Program, [String], BP, Symbol_table)
-- output of compile Exprs
-- type ExprOut = (String, IC_Program, BP, [String])
-- ****************************************************************************
-- helper functions:
getAdress :: String -> Symbol_table -> Integer
getAdress s table = case Data.Map.lookup s table of
                         Just (i,_,_) -> i
                         _ -> undefined
getOp :: (Op,Int) -> Op
getOp (a,b) = a
getStmts :: String -> Symbol_table -> [Stmts]
getStmts s table = case Data.Map.lookup s table of
                        Just (_,_,Block st) -> st 
                        _ -> undefined
-- functions for expressions
changeStatePlus :: Op -> Op -> StatefulUnsafe Compile_State (Op,Int)
changeStatePlus op1 op2 = StatefulUnsafe $ \(ic, (t:ts), bp, symbol) -> (Ok (Var' t,(length ic) + 1), (ic ++ [(Plus' (Var' t) op1 op2)], ts, bp, symbol))
changeStateMinus :: Op -> Op -> StatefulUnsafe Compile_State (Op,Int)
changeStateMinus op1 op2 = StatefulUnsafe $ \(ic, (t:ts), bp, symbol) -> (Ok (Var' t,(length ic) + 1), (ic ++ [(Minus' (Var' t) op1 op2)], ts, bp, symbol))
changeStateTimes :: Op -> Op -> StatefulUnsafe Compile_State (Op,Int)
changeStateTimes op1 op2 = StatefulUnsafe $ \(ic, (t:ts), bp, symbol) -> (Ok (Var' t,(length ic) + 1), (ic ++ [(Times' (Var' t) op1 op2)], ts, bp, symbol))
changeStateDiv :: Op -> Op -> StatefulUnsafe Compile_State (Op,Int)
changeStateDiv op1 op2 = StatefulUnsafe $ \(ic, (t:ts), bp, symbol) -> (Ok (Var' t,(length ic) + 1), (ic ++ [(Div' (Var' t) op1 op2)], ts, bp, symbol))
changeStateMod :: Op -> Op -> StatefulUnsafe Compile_State (Op,Int)
changeStateMod op1 op2 = StatefulUnsafe $ \(ic, (t:ts), bp, symbol) -> (Ok (Var' t,(length ic) + 1), (ic ++ [(Mod' (Var' t) op1 op2)], ts, bp, symbol))
changeStateLt :: Op -> Op -> StatefulUnsafe Compile_State (Op,Int)
changeStateLt op1 op2 = StatefulUnsafe $ \(ic, (t:ts), bp, symbol) -> (Ok (Var' t,(length ic) + 1), (ic ++ [(Lt' (Var' t) op1 op2)], ts, bp, symbol))
changeStateGt :: Op -> Op -> StatefulUnsafe Compile_State (Op,Int)
changeStateGt op1 op2 = StatefulUnsafe $ \(ic, (t:ts), bp, symbol) -> (Ok (Var' t,(length ic) + 1), (ic ++ [(Gt' (Var' t) op1 op2)], ts, bp, symbol))
changeStateLe :: Op -> Op -> StatefulUnsafe Compile_State (Op,Int)
changeStateLe op1 op2 = StatefulUnsafe $ \(ic, (t:ts), bp, symbol) -> (Ok (Var' t,(length ic) + 1), (ic ++ [(Le' (Var' t) op1 op2)], ts, bp, symbol))
changeStateGe :: Op -> Op -> StatefulUnsafe Compile_State (Op,Int)
changeStateGe op1 op2 = StatefulUnsafe $ \(ic, (t:ts), bp, symbol) -> (Ok (Var' t,(length ic) + 1), (ic ++ [(Ge' (Var' t) op1 op2)], ts, bp, symbol))
changeStateEq :: Op -> Op -> StatefulUnsafe Compile_State (Op,Int)
changeStateEq op1 op2 = StatefulUnsafe $ \(ic, (t:ts), bp, symbol) -> (Ok (Var' t,(length ic) + 1), (ic ++ [(Equal' (Var' t) op1 op2)], ts, bp, symbol))
changeStateNotEq :: Op -> Op -> StatefulUnsafe Compile_State (Op,Int)
changeStateNotEq op1 op2 = StatefulUnsafe $ \(ic, (t:ts), bp, symbol) -> (Ok (Var' t,(length ic) + 1), (ic ++ [(NotEq' (Var' t) op1 op2)], ts, bp, symbol))
changeStateUminus :: Op -> StatefulUnsafe Compile_State (Op,Int)
changeStateUminus op = StatefulUnsafe $ \(ic, (t:ts), bp, symbol) -> (Ok (Var' t,(length ic) + 1), (ic ++ [Uminus' (Var' t) op], ts, bp, symbol))
changeStateNot ::Op -> StatefulUnsafe Compile_State (Op,Int)
changeStateNot op = StatefulUnsafe $ \(ic, (t:ts), bp, symbol) -> (Ok (Var' t,(length ic) + 1), (ic ++ [Not' (Var' t) op], ts, bp, symbol))

-- insert function to symbol table
insertFunction :: String -> Expr -> Stmts -> StatefulUnsafe Compile_State Int
insertFunction s e st = StatefulUnsafe $ \(ic, t, bp , symbol) -> (Ok 81612345, (ic, t, bp, insert s (fromIntegral (length (ic)),e,st) symbol))

-- functions for statements
changeStateAssign :: String -> Op -> StatefulUnsafe Compile_State Int
changeStateAssign s op = StatefulUnsafe $ \(ic, t, bp, symbol) -> (Ok ((length ic) + 1), (ic ++ [Assign' (Var' s) op], t, bp, symbol))
changeStatePrint :: String -> StatefulUnsafe Compile_State Int
changeStatePrint s = StatefulUnsafe $ \(ic, t, bp, symbol) -> (Ok ((length ic) + 1), (ic ++ [Print' (s++" = ") (Var' s)], t, bp, symbol))
changeStateReturn :: Op -> StatefulUnsafe Compile_State Int
changeStateReturn op = StatefulUnsafe $ \(ic, t, bp, symbol) -> (Ok ((length ic) + 1),(ic ++ [Return' op], t, bp, symbol))
changeStateIf :: Op -> StatefulUnsafe Compile_State Int
changeStateIf op = StatefulUnsafe $ \(ic, t, (tr,fa,br,cn), symbol) -> (Ok ((length ic) + 2), (ic ++ [Bzero' op (fromIntegral 0), Jump' (fromIntegral ((length ic) + 2))], t,(tr,fa++[(length ic)],br,cn) ,symbol))
-- insertFunction s [x:xs] st = 
-- ****************************************************************************

compile :: Program -> IC_Program
compile p = compileFuncs p testC
compileFuncs :: Program -> Compile_State -> IC_Program
compileFuncs [] (ic,_,_,_) = ic
compileFuncs (x:xs) state = case app (compileStmt x) state of
                                 (_,newState) -> compileFuncs xs newState
compileMain :: Compile_State -> IC_Program
compileMain (ic, temp, bp, symbol) =  compileStmts (getStmts "main" symbol) (ic,temp,bp,symbol)
compileStmts :: [Stmts] -> Compile_State -> IC_Program
compileStmts [] (ic, temp, bp, symbol) =  ic
compileStmts (s:sx) (ic, temp, bp, symbol) = case app (compileStmt s) (ic, temp, bp, symbol) of
                                                  (_,(newIc, newTemp,newBp,_)) -> compileStmts sx (newIc,newTemp,newBp,symbol)
-- compileStList :: [Stmts] -> StatefulUnsafe Compile_State Int
-- compileStmts [] = return 0
-- compileStmts (x:xs) = do _ <- compileStmt x
--                          compileStmts xs
-- data Stmts = 
--              While Expr Stmts |
--              Block [Stmts] |
--              If Expr Stmts |
--              IfElse Expr Stmts Stmts |
--        Func String Expr Stmts |
--              Return Expr |
--              Print String |
--              Break |
--              Continue 
--              deriving Eq
-- --  TODO : Arg [Expr] eval?
-- data Expr = 
--             And Expr Expr |
--             Or Expr Expr |
--             Not Expr |
--             Arg [Expr] |
--       Call String Expr |
--       CallNoArg String

compileStmt :: Stmts -> StatefulUnsafe Compile_State Int
compileStmt (Assign s e) = do op <- compileExpr e
                              changeStateAssign s (getOp op)
compileStmt (Print s) = changeStatePrint s
compileStmt (FuncNoArg s stmt) = StatefulUnsafe $ \state -> case app (insertFunction s (Var "zrh") stmt) state of
                                                                    (_,(ic,temp,bp,symbol)) -> if (s == "main") then app (compileStmt stmt) (take 1 ic ++ [Call' (length ic)] ++ drop 2 ic,temp,bp,symbol) else
                                                                                                   app (compileStmt stmt) (ic,temp,bp,symbol) 
compileStmt (Func name args stmt) = StatefulUnsafe $ \state -> case app (insertFunction name args stmt) state of
                                                                    (_,newState) -> app (compileStmt stmt) newState
compileStmt (Return e) = do res <- compileExpr e
                            changeStateReturn (getOp res)
compileStmt (If e stmt) = StatefulUnsafe $ \state -> case app (compileExpr e) state of
                                                          (Ok (res,t_back),newState)-> case app (changeStateIf res) newState of
                                                                                   (Ok backtrack, (ic,temp,(t,f,b,c), symbol)) -> case app (compileStmt stmt) (backpatchingJump t backtrack ic, temp, ([],f,b,c),symbol) of
                                                                                                                                                     (Ok address,(ic,temp,(t,f,b,c),symbol)) -> (Ok address, (backpatching (f++[t_back]) address res ic,temp,(t,[],b,c),symbol))
compileStmt (Block []) = StatefulUnsafe $ \(ic, t, br, symbol) -> ((Ok (length ic)), (ic,t,br,symbol))
compileStmt (Block (s:sx)) =  do _ <- compileStmt s
                                 compileStmt (Block sx)                                                       
compileStmt (IfElse e st1 st2) = StatefulUnsafe $ \state -> case app (compileExpr e) state of
                                                                 (Ok (res,t_back),newState) -> case app (changeStateIf res) newState of
                                                                                               (_,newState2) -> case app (compileStmt st1) newState2 of
                                                                                                                     (Ok false_address,(ic,temp,(t2,f2,b2,c2),symbol)) -> case app (compileStmt st2) ((backpatching (f2++[t_back]) ((length ic) + 1) res (ic++[Jump' 0])),temp,(t2++[length ic],[],b2,c2),symbol) of
                                                                                                                                                                           (Ok true_address,(ic,temp,(t,f,b,c),symbol)) -> (Ok true_address, ((backpatchingJump (t++[false_address]) (length ic) ic),temp,([],f,b,c),symbol))
                                                                 
compileStmt (While e s) = StatefulUnsafe $ \state -> case app (compileExpr e) state of
                                                          (Ok (op,ex_address), (ic,temp,(t,f,b,c),symbol)) -> case app (compileStmt s) (backpatchingJump t ex_address ic ++ [Bzero' op 0,Jump' ((length ic) + 2)], temp, ([],f ++ [length ic],b,c),symbol) of
                                                                                                                   (Ok _,(ic,temp,(t,f,b,c),symbol)) -> (Ok (length ic + 1), (backpatchingJump c ex_address (backpatchingJump b (length ic + 1) (backpatching (f ++ [ex_address]) ((length ic) + 1) op (ic ++ [Jump' (ex_address - 1)]))),temp,(t,[],[],[]),symbol))
compileStmt Break = StatefulUnsafe $ \(ic, temp, (t,f,b,c), symbol) -> (Ok (length ic), (ic++[Jump' 0], temp, (t,f,b ++[length ic],c), symbol))
compileStmt Continue = StatefulUnsafe $ \(ic, temp, (t,f,b,c), symbol) -> (Ok (length ic), (ic++[Jump' 0], temp, (t,f,b,c ++ [length ic]), symbol))

compileStmt _ = undefined

-- add continue and break together
-- backpatching false list
backpatching :: [Int] -> Int -> Op-> IC_Program -> IC_Program
backpatching [] _ _ program = program
backpatching (x:xs) address op program = case (program !! x) of
                                              (Bzero' b c) -> backpatching xs address op (take x program ++ [Bzero' b address] ++ drop (x + 1) program)
backpatchingJump :: [Int] -> Int -> IC_Program -> IC_Program
backpatchingJump [] _ program = program
backpatchingJump (x:xs) address program = backpatchingJump xs address (take x program ++ [Jump' address] ++ drop (x+1) program)
compileExpr :: Expr -> StatefulUnsafe Compile_State (Op,Int)
compileExpr (Val i) = return (Val' (fromInteger i),0)
compileExpr (Var s) = return (Var' s,0)
compileExpr (Plus expr1 expr2) = do res1 <- compileExpr expr1
                                    res2 <- compileExpr expr2
                                    changeStatePlus (getOp res1) (getOp res2)
compileExpr (Minus expr1 expr2) = do res1 <- compileExpr expr1
                                     res2 <- compileExpr expr2
                                     changeStateMinus (getOp res1) (getOp res2)
compileExpr (Times expr1 expr2) = do res1 <- compileExpr expr1
                                     res2 <- compileExpr expr2
                                     changeStateTimes (getOp res1) (getOp res2)
compileExpr (Div expr1 expr2) = do res1 <- compileExpr expr1
                                   res2 <- compileExpr expr2
                                   changeStateDiv (getOp res1) (getOp res2)
compileExpr (Mod expr1 expr2) = do res1 <- compileExpr expr1
                                   res2 <- compileExpr expr2
                                   changeStateMod (getOp res1) (getOp res2)
compileExpr (Lt expr1 expr2) = do res1 <- compileExpr expr1
                                  res2 <- compileExpr expr2
                                  changeStateLt (getOp res1) (getOp res2)
compileExpr (Gt expr1 expr2) = do res1 <- compileExpr expr1
                                  res2 <- compileExpr expr2
                                  changeStateGt (getOp res1) (getOp res2)
compileExpr (Le expr1 expr2) = do res1 <- compileExpr expr1
                                  res2 <- compileExpr expr2
                                  changeStateLe (getOp res1) (getOp res2)
compileExpr (Ge expr1 expr2) = do res1 <- compileExpr expr1
                                  res2 <- compileExpr expr2
                                  changeStateGe (getOp res1) (getOp res2)
compileExpr (Eq expr1 expr2) = do res1 <- compileExpr expr1
                                  res2 <- compileExpr expr2
                                  changeStateEq (getOp res1) (getOp res2)
compileExpr (NotEq expr1 expr2) = do res1 <- compileExpr expr1
                                     res2 <- compileExpr expr2
                                     changeStateNotEq (getOp res1) (getOp res2)
compileExpr (NotEq expr1 expr2) = do res1 <- compileExpr expr1
                                     res2 <- compileExpr expr2
                                     changeStateNotEq (getOp res1) (getOp res2)
compileExpr (Rev expr) = do res <- compileExpr expr
                            changeStateUminus (getOp res)
--  and/or/not
compileExpr (And e1 e2) = do res1 <- compileExpr e1
                             changeStateAnd (getOp res1)
                             res2 <- compileExpr e2
                             changeStateAnd (getOp res2)
compileExpr (Or e1 e2) = do res1 <- compileExpr e1
                            changeStateOr (getOp res1)
                            res2 <- compileExpr e2
                            changeStateOr2 (getOp res2)
compileExpr (Not e) = do res <- compileExpr e
                         changeStateNot (getOp res)
compileExpr (CallNoArg s) = changeStateFuncNoArg s
compileExpr (Call s e) = do res <- compileExpr e
                            changeStateFunc s (getOp res)
compileExpr _ = undefined 

changeStateFuncNoArg :: String -> StatefulUnsafe Compile_State (Op,Int)
changeStateFuncNoArg s = StatefulUnsafe $ \(ic,(t:ts),bp,symbol) -> (Ok (Var' t,(length ic) + 3),(ic ++ [Push', Call' (getFuncAdd symbol s),Assign' (Var' t) (Var' "_ret_val")],ts,bp,symbol))

changeStateFunc :: String -> Op -> StatefulUnsafe Compile_State (Op,Int)
changeStateFunc s op = StatefulUnsafe $ \(ic,(t:ts),bp,symbol) -> (Ok (Var' t,(length ic) + 4),(ic ++ [Push', Assign' (Var' (getFuncArg symbol s)) op,Call' (getFuncAdd symbol s),Assign' (Var' t) (Var' "_ret_val")],ts,bp,symbol))

getFuncAdd :: Map String (Integer, Expr, Stmts) -> String -> Int
getFuncAdd m s = case Data.Map.lookup s m of
                      Just (i,_,_) -> fromIntegral i
getFuncArg :: Map String (Integer, Expr, Stmts) -> String -> String
getFuncArg m s = case Data.Map.lookup s m of
                      Just (_,Var e,_) -> e
changeStateAnd :: Op ->StatefulUnsafe Compile_State (Op,Int)
changeStateAnd op = StatefulUnsafe $ \(ic,temp,(t,f,b,c),symbol) -> (Ok (op,(length ic) + 2), ((ic ++ [Bzero' op 0, Jump' ((length ic) + 2)]),temp,(t,f++[length ic],b,c),symbol)) 
changeStateOr :: Op ->StatefulUnsafe Compile_State (Op,Int)
changeStateOr op = StatefulUnsafe $ \(ic,temp,(t,f,b,c),symbol) -> (Ok (op,(length ic) + 2), ((ic ++ [Bzero' op ((length ic) + 2), Jump' 0]),temp,(t++[(length ic) + 1],f,b,c),symbol)) 

changeStateOr2 :: Op ->StatefulUnsafe Compile_State (Op,Int)
changeStateOr2 op = StatefulUnsafe $ \(ic,temp,(t,f,b,c),symbol) -> (Ok (op,(length ic) + 2), (backpatching f (length ic - 2) op (ic ++ [Bzero' op 0, Jump' 0]),temp,(t++[(length ic) + 1],[length ic],b,c),symbol)) 

-- test_compile :: String -> IC_Program
test_compile s = compile (changeToProgram (parse parser s))




