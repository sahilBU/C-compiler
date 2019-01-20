Implemented a C compiler using Haskell. Wrote a Parser to convert the source language into Abstract Syntax Tree which was fed into compiler written in Haskell which gives the intermediate code. Intermediate code Interpreter used the intermediate code to output the Assembly language.

Parser  -> AST  ->  AST Interpreter ----------------------------------------------> Output (list of Strings)
                  |                                                                       ^
		  |---->  Compiler  ---> IC Program -------->  IC Interpreter (provided) -|
		                                               
     The grammar for the source language - 
     0: Program := Funcs
1: Funcs := Func Funcs
2: Funcs := Func 
3: Func := def identifier (  ) { Stmts } 
4: Func := def identifier ( identifier ) { Stmts } 
5: Stmts := Stmt ; Stmts 
6: Stmts := Stmt ; 
7: Stmts := Block Stmts 
8: Stmts := Block 
9:  Block := { Stmts } 
10: Block := while ( BExpr ) Block 
11: Block := if ( BExpr ) Block
12: Block := if ( BExpr ) Block else Block 
13: Stmt := id = Expr
14: Stmt := return Expr
15: Stmt := print identifier
16: Stmt := break 
17: Stmt := continue
18: BExpr := BTerm || BExpr
19: BExpr := BTerm
20: BTerm := BFactor && BTerm
21: BTerm := BFactor
22: BFactor := ! Bfactor
23: BFactor := Cond
24: Bfactor := ( Bexpr )
25: Cond := Expr == Expr
26: Cond := Expr != Expr
27: Cond := Expr < Expr
28: Cond := Expr <= Expr
29: Cond := Expr > Expr
30: Cond := Expr >= Expr
31: Expr := Expr + Term 
32: Expr := Expr - Term 
33: Expr := Term  
34: Term := Term * Factor 
35: Term := Term / Factor 
36: Term := Term % Factor 
37: Term := Factor 
38: Factor := - Factor 
39: Factor := identifier ( ) 
40: Factor := identifier ( Expr ) 
41: Factor := identifier 
42: Factor := integer 
43: Factor := ( Expr )
			                       
