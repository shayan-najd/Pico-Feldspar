%format bx_PP
%format obs_X
%format toList_Doc
%format index_I
\begin{comment}

> {-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
> {-# LANGUAGE ExplicitForAll  #-}

\end{comment}
This module provides the necessary functions to bidirectionalize the transformation from EDSL to C code by composing the bidirectionalization of each smaller transformations in between.
\begin{newcode}

> {-# LANGUAGE GADTs  #-}
> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
>   
> module Feldspar.BX  where
>
> import qualified Prelude as P
> import Prelude (String,Either(..),Maybe(..),Eq(..)
>                ,Read(..),Monad(..),map,zip,(.),($)) 
> import Data.Foldable(toList)

> import Feldspar.Types
> import Feldspar.FrontEnd.AST
> import Feldspar.Compiler.BXCompiler (BXable(..))
> import Feldspar.BackEnd.BXPretty (putPretty)
> import Feldspar.Compiler.Compiler (toFunc,compile)
> import Feldspar.BackEnd.Pretty(Pretty(..))
> import Feldspar.AnnotationUtils (PushDown(..)) 
> import Annotations (Inj(..))

> -- zipping similiar AST with different Annotations 
> class ZipData t t' where
>  zipData :: t -> t' -> [(Ann t,Ann t')]
> instance (SingT a, ZipData r r'
>          , Ann r ~ ann, Ann r' ~ ann') =>
>          ZipData (Data a ann ->r) 
>                  (Data a ann'->r') where
>  zipData f g = zipData 
>                (f $ Var $ VarT "_x" sing)
>                (g $ Var $ VarT "_x" sing) 
> instance ZipData (Data a ann) (Data a ann') where
>  zipData d d' =  zip (toList d) (toList d') 

> -- putting back changes up to the src-loc 
> putAnn :: forall t t'.
>  (PushDown t', BXable t, ZipData t t'
>  , Ann t ~ P.Bool) =>
>  P.Bool -> (t' -> t) -> t' -> String ->
>  Either String [Ann t']
> putAnn cn markA d src = do
>  let dS = pushDown Nothing d  
>  let dM = markA d
>  dU    <- put cn dM src       
>  return [ s|(b,s) <- zipData dU dS,b]

> -- putting back changes up to the high-level AST
> put :: forall b.
>             (Eq (Ann b), Read (Ann b),
>              Pretty (Ann b),BXable b) =>            
>           P.Bool -> b -> String -> 
>           Either String b
> put b s v' = do
>   let s'  = (toFunc . compile 0)  s  
>   let ps' = if b 
>             then pushDown Nothing s'
>             else s'       
>   v <- putPretty ps' v'
>   putCompile 0 s v  

\end{newcode} 