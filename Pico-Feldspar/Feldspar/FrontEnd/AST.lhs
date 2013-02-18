%format ann = "\colorbox{gray}{ann}"
%format e_1 
%format e_2
%format Lit_Int
%format Lit_Bool
%format Eq_Int
%format LT_Int
\begin{comment}

> {-# OPTIONS_GHC -Wall #-}

\end{comment}
This module, provides the type-safe representation (via GADTs) of the high-level language.

> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE KindSignatures #-}
>
> module Feldspar.FrontEnd.AST where
>
> import qualified Prelude as P
> import Feldspar.Types 

> -- AST of the EDSL (high-level)
> data Data (a :: Types) ann where
>   Var     :: VarT a -> Data a ann  
>   Lit_Int :: P.Int -> Data Int32 ann
>   Add :: Data Int32 ann -> Data Int32 ann -> Data Int32 ann 
>   Sub :: Data Int32 ann -> Data Int32 ann -> Data Int32 ann 
>   Mul :: Data Int32 ann -> Data Int32 ann -> Data Int32 ann 
>   Eq_Int :: Data Int32 ann -> Data Int32 ann -> Data Bool ann
>   LT_Int :: Data Int32 ann -> Data Int32 ann -> Data Bool ann
>   Lit_Bool :: P.Bool -> Data Bool ann
>   Not :: Data Bool ann -> Data Bool ann
>   And :: Data Bool ann -> Data Bool ann -> Data Bool ann
>   If :: Data Bool ann  -> Data a ann -> Data a ann -> Data a ann

\begin{newcode}

>   Ann :: ann -> Data a ann -> Data a ann

\end{newcode}

> -- Variables
> data VarT t =  VarT  P.String (SingTypes t)
 