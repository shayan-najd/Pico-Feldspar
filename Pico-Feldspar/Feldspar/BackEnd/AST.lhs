%format ann = "\colorbox{gray}{ann}"
\begin{comment}

> {-# OPTIONS_GHC -Wall #-}
> {-# LANGUAGE ExplicitForAll #-}

\end{comment}
This module contains the declaration of the AST of the low-level language (\emph{C}).

> module Feldspar.BackEnd.AST where
> 
> import qualified Prelude
> import Prelude (Int,String)
> import Feldspar.Types

> -- variables  
> type Var = (String,Types)
 
> -- C function             
> data Func ann = 
>    Func String [Var] [Stmt ann]  

> -- C statement 
> data Stmt ann = 
>    If_C (Exp_C ann) [Stmt ann] [Stmt ann]
>  | Assign String (Exp_C ann)
>  | Declare Types String
 
\begin{newcode}

>  | Ann_Stmt ann (Stmt ann)

\end{newcode}

> -- C expressions 
> data Exp_C ann = 
>    Var_C String
>  | Num Int
>  | Infix (Exp_C ann) String (Exp_C ann)
>  | Unary String (Exp_C ann)

\begin{newcode}

>  | Ann_Exp_C ann (Exp_C ann)

\end{newcode}
