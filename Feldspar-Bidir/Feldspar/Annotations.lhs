> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE FlexibleInstances #-}

> module Feldspar.Annotations (module Annotations) where

> import Annotations
> import Feldspar.FrontEnd.AST
> import Feldspar.BackEnd.AST

> instance Inj (Data a ann) where
>  type Ann (Data a ann) = ann
>  inj x = Ann x 

> instance Annotatable (Data a ann) where
>  prj (Ann x e) = Just (x,e)
>  prj _         = Nothing

> instance Inj (Exp_C ann) where
>  type Ann (Exp_C ann) = ann
>  inj x = Ann_Exp_C x 

> instance Annotatable (Exp_C ann) where
>  prj (Ann_Exp_C x e) = Just (x,e)
>  prj _               = Nothing

> instance Inj (Stmt ann) where
>  type Ann (Stmt ann) = ann
>  inj x = Ann_Stmt x

> instance Annotatable (Stmt ann) where
>  prj (Ann_Stmt x e) = Just (x,e)
>  prj _              = Nothing

> instance Inj (Int,(Exp_C ann,Types,[Stmt ann])) where
>  type Ann (Int,(Exp_C ann,Types,[Stmt ann])) = ann
>  inj x  (i,(e,t,sts)) = (i,(inj x e,t,inj x `map` sts)) 

> instance Inj r => Inj (a -> r) where
>  type Ann (a -> r) = Ann r
>  inj ann f = \x -> inj ann (f x)  


