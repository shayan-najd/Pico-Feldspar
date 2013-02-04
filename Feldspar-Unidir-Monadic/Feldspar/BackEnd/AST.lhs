\ignore{

> {-# OPTIONS_GHC -Wall #-}
> {-# LANGUAGE ExplicitForAll #-}

} 

> module Feldspar.BackEnd.AST where

> data Types = TInt32 | TBool

> type Var = (String,Types)

> data Func = Func String [Var] [Stmt]  

> data Stmt = 
>    If_C Exp_C [Stmt] [Stmt]
>  | Assign String Exp_C
>  | Declare Types String
  
> data Exp_C = 
>    Var_C String
>  | Num Int
>  | Infix Exp_C String Exp_C
>  | Unary String Exp_C
>  deriving Eq

