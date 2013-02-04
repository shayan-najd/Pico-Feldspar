%format e_1 
%format e_2
%format Lit_Int
%format Lit_Bool
%format Eq_Int
%format LT_Int

> {-# OPTIONS_GHC -Wall #-}
> {-# LANGUAGE GADTs #-}

> module Feldspar.FrontEnd.AST where

> data Data a ann where
>   Var     :: String -> Data a ann  
>   Lit_Int :: Int32 -> Data Int32 ann
>   Add :: Data Int32 ann -> Data Int32 ann -> Data Int32 ann 
>   Sub :: Data Int32 ann -> Data Int32 ann -> Data Int32 ann 
>   Mul :: Data Int32 ann -> Data Int32 ann -> Data Int32 ann 
>   Eq_Int :: Data Int32 ann -> Data Int32 ann -> Data Bool ann
>   LT_Int :: Data Int32 ann -> Data Int32 ann -> Data Bool ann
>   Lit_Bool :: Bool -> Data Bool ann
>   Not :: Data Bool ann -> Data Bool ann
>   And :: Data Bool ann -> Data Bool ann -> Data Bool ann
>   If :: Data Bool ann  -> Data a ann -> Data a ann -> Data a ann
>   Ann :: ann -> Data a ann -> Data a ann

> type Int32 = Int