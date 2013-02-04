> {-# OPTIONS_GHC -Wall  #-}
> {-# LANGUAGE ExplicitForAll #-}

> {-# LANGUAGE FlexibleInstances #-}

> module Feldspar.FrontEnd.Interface where

> import qualified Prelude
> import Prelude (Num(..),Int,Bool(..),($),Show,String)

> import Feldspar.FrontEnd.AST

> instance Num (Data Int32) where
>  fromInteger i = Lit_Int $ fromInteger i
>  (+) = Add
>  (-) = Sub
>  (*) = Mul
>  signum x = condition (x < 0) 
>                       (-1) 
>                       (condition (x == 0) 0 1) 
>  abs x = (signum x) * x

> (==) :: Data Int32 -> Data Int32 -> Data Bool
> (==)      = Eq_Int 

> (<)  :: Data Int32 -> Data Int32 -> Data Bool
> (<)       = LT_Int

> (>) :: Data Int32 -> Data Int32 -> Data Bool
> e_1 > e_2 = not $ e_1 < e_2

> (<=) :: Data Int32 -> Data Int32 -> Data Bool
> e_1 <= e_2 = (e_1 < e_2) && (e_1 == e_2)

> (>=) :: Data Int32 -> Data Int32 -> Data Bool
> e_1 >= e_2 = (e_1 > e_2) && (e_1 == e_2)

> true :: Data Bool
> true = Lit_Bool True  

> false :: Data Bool
> false = Lit_Bool False

> not :: Data Bool -> Data Bool
> not = Not

> (&&) :: Data Bool -> Data Bool -> Data Bool
> (&&) = And

> (||) :: Data Bool -> Data Bool -> Data Bool
> x || y = not ((not x) && (not y))

> condition :: forall a. Data Bool -> Data a -> 
>              Data a -> Data a
> condition = If
