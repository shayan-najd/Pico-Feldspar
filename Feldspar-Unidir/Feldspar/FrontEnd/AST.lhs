%format e_1 
%format e_2
%format Lit_Int
%format Lit_Bool
%format Eq_Int
%format LT_Int

> {-# OPTIONS_GHC -Wall #-}
> {-# LANGUAGE GADTs #-}

> module Feldspar.FrontEnd.AST where

> data Data a where
>   Var     :: String -> Data a  
>   Lit_Int :: Int32 -> Data Int32
>   Add :: Data Int32 -> Data Int32 -> Data Int32 
>   Sub :: Data Int32 -> Data Int32 -> Data Int32 
>   Mul :: Data Int32 -> Data Int32 -> Data Int32 
>   Eq_Int :: Data Int32 -> Data Int32 -> Data Bool
>   LT_Int :: Data Int32 -> Data Int32 -> Data Bool
>   Lit_Bool :: Bool -> Data Bool
>   Not :: Data Bool -> Data Bool
>   And :: Data Bool -> Data Bool -> Data Bool
>   If :: Data Bool  -> Data a -> Data a -> Data a

> type Int32 = Int