\ignore{

> {-# OPTIONS_GHC -Wall #-}
> {-# LANGUAGE ExplicitForAll #-}

} 

> module Feldspar.BackEnd.AST where

> data Types = TInt32 | TBool
>  deriving Eq

> type Var = (String,Types)

> data Func ann = 
>    Func String [Var] [Stmt ann]  

> data Stmt ann = 
>    If_C (Exp_C ann) [Stmt ann] [Stmt ann]
>  | Assign String (Exp_C ann)
>  | Declare Types String
>  | Ann_Stmt ann (Stmt ann)
>  deriving Eq

> data Exp_C ann = 
>    Var_C String
>  | Num Int
>  | Infix (Exp_C ann) String (Exp_C ann)
>  | Unary String (Exp_C ann)
>  | Ann_Exp_C ann (Exp_C ann)
>  deriving Eq

