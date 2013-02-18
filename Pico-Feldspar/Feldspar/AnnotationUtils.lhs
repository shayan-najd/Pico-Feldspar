%format bx_PP
%format obs_X
%format toList_Doc
%format index_I
\begin{comment}

> {-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
> {-# LANGUAGE ExplicitForAll  #-}

\end{comment}
This module provides a set of utilities to work with annotations, e.g., removing all the annotations from an AST |stripAnn| or annotating every single node in an AST with the value |False|.
\begin{newcode}

> {-# LANGUAGE GADTs  #-}
> {-# LANGUAGE FlexibleInstances  #-}   
> 
> module Feldspar.AnnotationUtils  where
>
> import qualified Prelude as P
> import Prelude (Maybe(..),map,($),(.)) 
                       
> import Feldspar.FrontEnd.AST
> import Feldspar.BackEnd.AST

> import Annotations (Inj(..)) 
> import Feldspar.Annotations ()

> -- removing all the annotations 
> stripAnn :: Data a ann -> Data a ann'
> stripAnn (Var         x) = Var      x
> stripAnn (Lit_Int     i) = Lit_Int  i
> stripAnn (Lit_Bool    b) = Lit_Bool b
> stripAnn (Not         e) = Not (stripAnn  e)  
> stripAnn (Add     e1 e2) = 
>  Add (stripAnn e1) (stripAnn e2)
> stripAnn (Sub     e1 e2) =
>  Sub (stripAnn e1) (stripAnn e2)  
> stripAnn (Mul     e1 e2) = 
>  Mul (stripAnn e1) (stripAnn e2)  
> stripAnn (Eq_Int  e1 e2) = 
>  Eq_Int (stripAnn e1) (stripAnn e2)  
> stripAnn (LT_Int  e1 e2) = 
>  LT_Int (stripAnn e1) (stripAnn e2)  
> stripAnn (And     e1 e2) = 
>  And (stripAnn e1) (stripAnn e2)  
> stripAnn (If   e1 e2 e3) = 
>  If (stripAnn e1) (stripAnn e2) (stripAnn e3)
> stripAnn (Ann _ e)       = stripAnn e

> -- annotating each node in the output with @False@  
> markAllF :: forall a ann ann' r. 
>             (r      ann' -> r P.Bool) ->
>             (Data a ann  -> r ann'  ) ->
>             (Data a P.Bool  -> r P.Bool)
> markAllF markAllr f = markAllr . f . stripAnn

> -- annotating each node with  @False@    
> markAll :: forall a ann. Data a ann 
>            -> Data a P.Bool  
> markAll (Var         x) = Ann P.False $
>   Var      x
> markAll (Lit_Int     i) = Ann P.False $ 
>   Lit_Int  i
> markAll (Lit_Bool    b) = Ann P.False $
>   Lit_Bool b
> markAll (Not         e) = Ann P.False $ 
>   Not (markAll  e)  
> markAll (Add     e1 e2) = Ann P.False $ 
>   Add (markAll e1) (markAll e2)
> markAll (Sub     e1 e2) = Ann P.False $ 
>   Sub (markAll e1) (markAll e2)
> markAll (Mul     e1 e2) = Ann P.False $ 
>   Mul (markAll e1) (markAll e2)
> markAll (Eq_Int  e1 e2) = Ann P.False $ 
>   Eq_Int (markAll e1) (markAll e2)
> markAll (LT_Int  e1 e2) = Ann P.False $ 
>   LT_Int (markAll e1) (markAll e2)
> markAll (And     e1 e2) = Ann P.False $ 
>   And (markAll e1) (markAll e2)
> markAll (If   e1 e2 e3) = Ann P.False $
>   If (markAll e1) (markAll e2) (markAll e3)
> markAll (Ann _ e)       =  
>   markAll e

> -- helper function 
> annCond :: forall k. Inj k => 
>             Maybe (Ann k) -> k -> k
> annCond (Just ann) e = inj ann e
> annCond Nothing    e = e

> -- pushing down the annotation, so the unannotated 
> -- nodes inherit the parent's annotation
> class PushDown t where
>  pushDown :: (Maybe (Ann t)) ->
>              t -> t 

> -- pushing down the annotations for functions
> instance PushDown r => 
>          PushDown (Data a ann -> r) where
>  pushDown ann f = pushDown ann . f

> -- pushing down the annotation for terms of 
> -- type @Data a ann@
> instance PushDown (Data a ann) where  
>  pushDown ann (Var         x) = annCond ann $
>    Var      x
>  pushDown ann (Lit_Int     i) = annCond ann $ 
>    Lit_Int  i
>  pushDown ann (Lit_Bool    b) = annCond ann $
>    Lit_Bool b
>  pushDown ann (Not         e) = annCond ann $ 
>    Not (pushDown ann  e)  
>  pushDown ann (Add     e1 e2) = annCond ann $ 
>    Add (pushDown ann e1) (pushDown ann e2)
>  pushDown ann (Sub     e1 e2) = annCond ann $ 
>    Sub (pushDown ann e1) (pushDown ann e2)
>  pushDown ann (Mul     e1 e2) = annCond ann $ 
>    Mul (pushDown ann e1) (pushDown ann e2)
>  pushDown ann (Eq_Int  e1 e2) = annCond ann $ 
>    Eq_Int (pushDown ann e1) (pushDown ann e2)
>  pushDown ann (LT_Int  e1 e2) = annCond ann $ 
>    LT_Int (pushDown ann e1) (pushDown ann e2)
>  pushDown ann (And     e1 e2) = annCond ann $ 
>    And (pushDown ann e1) (pushDown ann e2)
>  pushDown ann (If   e1 e2 e3) = annCond ann $
>    If (pushDown ann e1) (pushDown ann e2) 
>       (pushDown ann e3)
>  pushDown _ (Ann ann e)       = 
>    pushDown (Just ann) e 

> -- pushing down the annotation for terms of 
> -- type @Exp_C ann@
> instance PushDown (Exp_C ann) where
>  pushDown ann (Var_C x)       = annCond ann $
>    Var_C x
>  pushDown ann (Num i)         = annCond ann $
>    Num i
>  pushDown ann (Infix e1 x e2) = annCond ann $
>    Infix (pushDown ann e1) x (pushDown ann e2) 
>  pushDown ann (Unary x e)     = annCond ann $
>    Unary x (pushDown ann e)
>  pushDown _ (Ann_Exp_C ann e) =   
>    pushDown (Just ann) e 

> -- pushing down the annotation for terms of 
> -- type @Stmt ann@ 
> instance PushDown (Stmt ann) where
>  pushDown ann (If_C e stmts1 stmts2) = annCond ann $
>    If_C (pushDown ann e) (pushDown ann `map` stmts1) 
>                          (pushDown ann `map` stmts2)
>  pushDown ann (Assign x e)           = annCond ann $
>    Assign x (pushDown ann e)
>  pushDown ann (Declare t x)          = annCond ann $
>    Declare t x
>  pushDown _ (Ann_Stmt ann stmt)      =  
>    pushDown (Just ann) stmt

> -- pushing down the annotation for terms of 
> -- type @Func ann@ 
> instance PushDown (Func ann) where 
>  pushDown ann (Func x vs stmts) = Func x vs  $
>    pushDown ann `map` stmts  

\end{newcode}