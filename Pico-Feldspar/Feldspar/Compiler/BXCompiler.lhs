\begin{comment}

> {-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
> {-# LANGUAGE ExplicitForAll #-}

\end{comment}
This module contains the code to bidirectionalize the compile functions.
\begin{newcode}

> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE GADTs #-}
>
> module Feldspar.Compiler.BXCompiler where
>
> import qualified Prelude as P
> import Prelude (String,Eq(..),Either(..),Int,const
>                ,Monad(..),(.),tail,Show(..),(+)
>                ,(++),($))

> import BX
> import Annotations 
> import Feldspar.Types 
> import Feldspar.BackEnd.AST
> import Feldspar.FrontEnd.AST
> import Feldspar.Compiler.Compiler
> import Feldspar.FrontEnd.Derivings ()
> import Feldspar.BackEnd.Derivings ()


> -- overloaded function to bidirectionalize
> -- instances of @Compilable@
> class Compilable t => BXable t where
>   putCompile :: Eq (Ann t) => Int -> 
>                 t -> Func (Ann t) ->
>                 Either String t

> -- Bidirectionalization done by @bff_GUS_G_Gen@
> instance SingT a => BXable (Data a ann) where
>  putCompile i = bff_GUS_G_Gen 
>     (const (toFunc . compile i)) 
>     (const () :: forall ann. Tuple_1 ann -> ()) 

> -- Bidirectionalization done manually 
> instance (SingT a, BXable r
>          ,Ann r ~ ann,Abstract r) => 
>          BXable (Data a ann -> r) where
>  putCompile i f (Func x ps stmts) = do 
>   let n = "v" ++ (show i)  
>       vt= VarT n (getTypeF f)
>       v = (Var vt) 
>       r = f v 
>   r' <- putCompile (i+1) r (Func x  (tail ps)  stmts) 
>   return $ \vv -> abstract vt vv r'    
 
> -- overloaded function to abstract over 
> -- a variable and generate the parametric AST
> class Abstract t where
>  abstract :: forall a. VarT a ->
>              Data a (Ann t) -> t -> t
 
> instance Abstract r =>
>          Abstract (Data a ann -> r) where
>  abstract vt d f = abstract vt d . f
 
> instance Abstract (Data a ann) where
>  abstract (VarT v SBool) d 
>         e@(Var (VarT x SBool)) 
>   | v == x = d  
>   | P.True = e
>  abstract (VarT v SInt32) d 
>         e@(Var (VarT x SInt32)) 
>   | v == x = d  
>   | P.True = e
>  abstract _ _ e@(Var _) = e 

>  abstract _ _ (Lit_Int    i ) = 
>   Lit_Int i

>  abstract _ _ (Lit_Bool   b ) = 
>   Lit_Bool b 

>  abstract v d (Not        e ) = 
>   Not     (abstract v d e )

>  abstract v d (Ann     a  e ) = 
>   Ann a   (abstract v d e )

>  abstract v d (Add     e1 e2) = 
>   Add     (abstract v d e1) 
>           (abstract v d e2)

>  abstract v d (Sub     e1 e2) = 
>   Sub     (abstract v d e1) 
>           (abstract v d e2)

>  abstract v d (Mul     e1 e2) = 
>   Mul     (abstract v d e1) 
>           (abstract v d e2)

>  abstract v d (Eq_Int  e1 e2) = 
>   Eq_Int  (abstract v d e1) 
>           (abstract v d e2)

>  abstract v d (LT_Int  e1 e2) = 
>   LT_Int  (abstract v d e1) 
>           (abstract v d e2)

>  abstract v d (And     e1 e2) = 
>   And     (abstract v d e1) 
>           (abstract v d e2)

>  abstract v d (If   e1 e2 e3) = 
>   If      (abstract v d e1) 
>           (abstract v d e2) 
>           (abstract v d e3)

\end{newcode} 