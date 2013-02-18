%format ann = "\colorbox{gray}{ann}"
%format Exp_C
%format If_C
%format Lit_I
%{
%format Ann (t) = "\colorbox{gray}{Ann\;\ensuremath{"t"}}"
%format Inj (t) = "\colorbox{gray}{Inj\;\ensuremath{"t"}}"
\begin{comment}

> {-# OPTIONS_GHC -Wall #-}
> {-# LANGUAGE ExplicitForAll #-}

\end{comment}
This module, contains the main code for compiling the high-level AST to C code.

> {-# LANGUAGE GADTs #-} 
> {-# LANGUAGE TypeSynonymInstances #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE FlexibleContexts #-}
>
> module Feldspar.Compiler.Compiler where
>
> import qualified Prelude as P
> import Prelude ((.),Show(..),putStrLn,IO
>                ,Int,String,(++),(+),Monad(..))
> import Control.Monad.State (State,put,get
>                            ,evalState)

> import Feldspar.Types
> import Feldspar.FrontEnd.AST 
> import Feldspar.BackEnd.AST
> import Feldspar.BackEnd.Pretty

\begin{newcode} 

> import Feldspar.Annotations

\end{newcode}

> -- the monadic function to compile the  
> -- the high-level AST to a pair containing
> -- an expression containing the returned 
> -- value and a list of statements; the 
> -- state contains a counter to generate
> -- fresh variables
> compileM :: SingT a => Data a ann -> 
>             State Int (Exp_C ann,[Stmt ann])

> compileM (Var (VarT v _)) =   
>    return (Var_C v      , [])
>
> compileM (Lit_Int x)      = 
>    return (Num x        , [])
>
> compileM (Lit_Bool P.True)  =  
>    return (Var_C "true" , []) 
>
> compileM (Lit_Bool P.False) = 
>    return (Var_C "false", [])
 
> compileM (Add e_1 e_2)    = do
>    (e_c_1,st_1) <- compileM e_1
>    (e_c_2,st_2) <- compileM e_2
>    return (Infix e_c_1 "+"  e_c_2 
>           , st_1 ++ st_2)

> compileM (Sub e_1 e_2)    = do
>    (e_c_1,st_1) <- compileM e_1
>    (e_c_2,st_2) <- compileM e_2
>    return (Infix e_c_1 "-"  e_c_2 
>           , st_1 ++ st_2)

> compileM (Mul e_1 e_2)    = do
>    (e_c_1,st_1) <- compileM e_1
>    (e_c_2,st_2) <- compileM e_2
>    return (Infix e_c_1 "*"  e_c_2 
>           , st_1 ++ st_2)

> compileM (Eq_Int e_1 e_2) = do
>    (e_c_1,st_1) <- compileM e_1
>    (e_c_2,st_2) <- compileM e_2
>    return (Infix e_c_1 "==" e_c_2 
>           , st_1 ++ st_2)

> compileM (LT_Int e_1 e_2) = do
>    (e_c_1,st_1) <- compileM e_1
>    (e_c_2,st_2) <- compileM e_2
>    return (Infix e_c_1 "<"  e_c_2 
>           , st_1 ++ st_2)

> compileM (And e_1 e_2)    = do
>    (e_c_1,st_1) <- compileM e_1
>    (e_c_2,st_2) <- compileM e_2
>    return (Infix e_c_1 "&&" e_c_2 
>           , st_1 ++ st_2)

> compileM (Not e_1)        = do 
>    (e_c_1,st_1) <- compileM e_1
>    return (Unary "!" e_c_1
>           , st_1)

> compileM e@(If e_1 e_2 e_3) = do
>    i <- get 
>    put (i+1)
>    let v = "v" ++ (show  i)    
>    (e_c_1,st_1) <- compileM e_1
>    (e_c_2,st_2) <- compileM e_2
>    (e_c_3,st_3) <- compileM e_3 
>    return 
>      (Var_C v 
>      , st_1 ++
>        [Declare (getType e) v
>        ,If_C e_c_1 
>          (st_2 ++ [Assign v e_c_2])
>          (st_3 ++ [Assign v e_c_3])])

\begin{newcode}

> compileM e = preserve e compileM

\end{newcode}

> -- overloaded function to compile  
> -- regardless of AST being parametric
> class Inj (t) =>         
>       Compilable t where
>     compileF :: ([Var], t) ->
>                State Int 
>                ([Var],Types
>                ,Exp_C (Ann (t))
>                ,[Stmt (Ann (t))])

> -- a parametric AST is first applied to
> -- a fresh variable with the right type      
> -- and then it is compiled
> instance (SingT a ,Compilable r) =>
>          Compilable (Data a ann' -> r) where
>    compileF (ps,f) = do
>      i <- get
>      put (i+1)
>      let v = "v" ++ (show i)
>          a = Var (VarT v (getTypeF f))   
>          r = f a
>      compileF ((ps ++ [(v,getType a)]),r)

> -- a non-parametric AST is compiled in  
> -- the normal way defined in @compileM@
> instance SingT a => 
>          Compilable (Data a ann) where
>  compileF (ps,d) = do 
>    (e,sts) <- compileM d  
>    return (ps,getType d,e,sts)

> -- coversion to @Func@
> toFunc :: ([Var],Types,Exp_C ann,[Stmt ann]) ->
>           Func ann
> toFunc (ps,ty,exp_C,stmts) = 
>  Func "test" (ps ++ [("*out",ty)])  
>  (stmts ++ [Assign "*out" exp_C])

> -- running the state monad with a seed
> compile :: Compilable a => Int ->
>            a -> ([Var],Types,Exp_C (Ann (a))
>                 ,[Stmt (Ann (a))]) 
> compile seed d = evalState (compileF ([],d)) seed

> -- an interface to the compiler  
> scompile :: (Compilable a, Pretty (Ann (a))) =>
>             a -> String  
> scompile = show . pretty . toFunc . (compile 0)

> -- an interface to the compiler 
> icompile :: (Compilable a, Pretty (Ann a)) =>
>             a -> IO ()
> icompile = putStrLn . scompile

%}