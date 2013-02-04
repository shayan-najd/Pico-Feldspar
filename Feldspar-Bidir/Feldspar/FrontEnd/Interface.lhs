> {-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
> {-# LANGUAGE ExplicitForAll #-}

> {-# LANGUAGE FlexibleInstances #-}

> module Feldspar.FrontEnd.Interface where

> import qualified Prelude
> import Prelude (Num(..),Int,Bool(..),($),Show,String)

> import Feldspar.FrontEnd.AST

> instance Num (Data Int32 ann) where
>  fromInteger i = Lit_Int $ fromInteger i
>  (+) = Add
>  (-) = Sub
>  (*) = Mul
>  signum x = condition (x < 0) 
>                       (-1) 
>                       (condition (x == 0) 0 1) 
>  abs x = (signum x) * x

> (==) :: forall ann. Data Int32 ann -> Data Int32 ann -> 
>         Data Bool ann
> (==)      = Eq_Int 

> (<)  :: forall ann. Data Int32 ann -> Data Int32 ann ->
>         Data Bool ann
> (<)       = LT_Int

> (>)  :: forall ann. Data Int32 ann -> Data Int32 ann -> 
>         Data Bool ann
> e_1 > e_2 = not $ e_1 < e_2

> (<=) :: forall ann. Data Int32 ann -> Data Int32 ann ->
>         Data Bool ann
> e_1 <= e_2 = (e_1 < e_2) && (e_1 == e_2)

> (>=) :: forall ann. Data Int32 ann -> Data Int32 ann ->
>         Data Bool ann
> e_1 >= e_2 = (e_1 > e_2) && (e_1 == e_2)

> true :: forall ann. Data Bool ann
> true = Lit_Bool True  

> false :: forall ann. Data Bool ann
> false = Lit_Bool False

> not :: forall ann. Data Bool ann -> Data Bool ann
> not = Not

> (&&) :: forall ann. Data Bool ann -> Data Bool ann -> 
>         Data Bool ann
> (&&) = And

> (||) :: forall ann. Data Bool ann -> Data Bool ann -> 
>         Data Bool ann
> x || y = not ((not x) && (not y))

> condition :: forall a ann. Data Bool ann -> Data a ann -> 
>              Data a ann -> Data a ann
> condition = If
