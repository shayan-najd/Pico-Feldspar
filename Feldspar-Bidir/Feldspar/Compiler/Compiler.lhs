%format Exp_C
%format If_C
%format Lit_I
\ignore{

> {-# OPTIONS_GHC -Wall #-}
> {-# LANGUAGE ExplicitForAll #-}

}

> {-# LANGUAGE GADTs #-} 
> {-# LANGUAGE TypeSynonymInstances #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE FlexibleContexts #-}

> module Feldspar.Compiler.Compiler where

> import Feldspar.FrontEnd.AST 
> import Feldspar.BackEnd.AST
> import Feldspar.BackEnd.Pretty
> import Feldspar.Annotations

> class GetType a where
>   getType :: Data a ann -> Types

> instance GetType Int32 where
>   getType _ = TInt32 

> instance GetType Bool where
>   getType _ = TBool

> compileD :: GetType a => Int -> Data a ann -> 
>            (Int,(Exp_C ann,Types,[Stmt ann]))
> compileD i e@(Var v)            = 
>    (i,(Var_C v      , getType e ,[]))
> compileD i e@(Lit_Int x)        =   
>    (i,(Num x        , getType e ,[]))
> compileD i e@(Lit_Bool True)    =  
>    (i,(Var_C "true" , getType e, [])) 
> compileD i e@(Lit_Bool False)   = 
>    (i,(Var_C "false", getType e, []))
> compileD i e@(Add e_1 e_2)      = let
>    (i',(e_c_1,_,st_1))   = compileD i  e_1
>    (i'',(e_c_2,_,st_2))  = compileD i' e_2
>    in (i'', (Infix e_c_1 "+"  e_c_2 
>             , getType e
>             , st_1 ++ st_2))
> compileD i e@(Sub e_1 e_2)      = let
>    (i',(e_c_1,_,st_1))   = compileD i  e_1
>    (i'',(e_c_2,_,st_2))  = compileD i' e_2
>    in (i'', (Infix e_c_1 "-"  e_c_2 
>             , getType e
>             , st_1 ++ st_2))
> compileD i e@(Mul e_1 e_2)      = let
>    (i',(e_c_1,_,st_1))   = compileD i  e_1
>    (i'',(e_c_2,_,st_2))  = compileD i' e_2
>    in (i'', (Infix e_c_1 "*"  e_c_2 
>             , getType e
>             , st_1 ++ st_2))
> compileD i e@(Eq_Int e_1 e_2)   = let
>    (i',(e_c_1,_,st_1))   = compileD i  e_1
>    (i'',(e_c_2,_,st_2))  = compileD i' e_2
>    in (i'', (Infix e_c_1 "=="  e_c_2 
>             , getType e
>             , st_1 ++ st_2))
> compileD i e@(LT_Int e_1 e_2)   = let
>    (i',(e_c_1,_,st_1))   = compileD i  e_1
>    (i'',(e_c_2,_,st_2))  = compileD i' e_2
>    in (i'', (Infix e_c_1 "<"  e_c_2 
>             , getType e
>             , st_1 ++ st_2))
> compileD i e@(And e_1 e_2)     = let
>    (i',(e_c_1,_,st_1))   = compileD i  e_1
>    (i'',(e_c_2,_,st_2))  = compileD i' e_2
>    in (i'', (Infix e_c_1 "&&"  e_c_2 
>             , getType e
>             , st_1 ++ st_2))
> compileD i e@(Not e_1)          = let 
>    (i',(e_c_1,_,st_1)) = compileD i e_1
>    in (i',(Unary "!"  e_c_1,getType e,st_1))
> compileD i e@(If e_1 e_2 e_3)   = let 
>    v = "v" ++ (show  i)    
>    (i'  ,(e_c_1,_,st_1)) = compileD (i+1) e_1
>    (i'' ,(e_c_2,_,st_2)) = compileD i'    e_2
>    (i''',(e_c_3,_,st_3)) = compileD i''   e_3 
>    in (i''', ( Var_C v 
>              , getType e
>              , st_1
>                ++ [Declare (getType e) v
>                   ,If_C e_c_1 
>                       (st_2 ++ [Assign v e_c_2])
>                       (st_3 ++ [Assign v e_c_3])]))
> compileD i e = preserve e (compileD i)       

> class Inj t => Compilable t where
>  compile :: [Var] -> Int -> t ->
>             ([Var],(Int,(Exp_C (Ann t) ,Types,[Stmt (Ann t)])))
  
> instance (GetType a ,Compilable r,Inj r,Ann r ~ ann) => 
>          Compilable (Data a ann -> r) where
>  compile ps i f = let
>   v = "v" ++ (show i)
>   a = Var v  
>   r = f a
>   in compile (ps ++ [(v,getType a)]) (i+1) r

> instance GetType a => 
>          Compilable (Data a ann) where
>  compile ps i d = (ps,compileD i d)

> toFunc :: ([Var],(Int,(Exp_C ann,Types,[Stmt ann]))) ->
>                    Func ann
> toFunc (ps,(_,(exp_C,ty,stmts))) = 
>  Func "test" (ps ++ [("*out",ty)])  
>  (stmts ++ [Assign "*out" exp_C])

> scompile :: (Inj a, Compilable a,Pretty (Ann a)) =>
>             a -> String  
> scompile = show . pretty . toFunc . (compile [] 0)

> icompile :: (Inj a,Compilable a,Pretty (Ann a)) => 
>             a -> IO ()
> icompile = putStrLn . scompile