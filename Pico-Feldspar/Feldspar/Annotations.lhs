\begin{comment}

> {-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
> {-# LANGUAGE ExplicitForAll  #-}

\end{comment}
In this module, the type classes defined in the module |Annotations| are derived for the main data types in Pico-Feldspar.
\begin{newcode}

> -- the code is omitted

\begin{comment}

> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE FlexibleInstances #-}
>  
> module Feldspar.Annotations 
>         (module Annotations) where
>
> import qualified Prelude as P
> import Prelude (map,Int,Maybe(..),String) 

> import qualified QuickAnnotate as QA 
> import Feldspar.FrontEnd.AST
> import Feldspar.BackEnd.AST
> import Annotations (Inj(..),Annotatable(..),preserve)
> import Feldspar.BackEnd.Pretty(Pretty(..))

> import Control.Monad.State
> import Text.PrettyPrint(text)

> instance QA.Annotatable (Data a String) where
>  annotate loc d = inj loc d

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

> instance Inj (Func ann) where
>  type Ann (Func ann) = ann
>  inj ann (Func x ps stmts) = 
>    Func x ps (inj ann `map` stmts)

> instance Inj t => Inj [t] where
>  type Ann [t] = Ann t
>  inj x = map (inj x)  

> instance (Inj t1,Inj t2,Ann t1 ~ Ann t2) =>
>           Inj (t1,t2) where
>  type Ann (t1,t2) = Ann t1
>  inj x (e_1,e_2) = (inj x e_1,inj x e_2)
                       
> instance Inj t => Inj (State Int t) where
>  type Ann (State Int t) = Ann t
>  inj x = fmap (inj x) 
 
> instance Inj r => Inj (a -> r) where
>  type Ann (a -> r) = Ann r
>  inj ann f = \x -> inj ann (f x)  

> instance Pretty String where
>  pretty = text

\end{comment}
\end{newcode}
