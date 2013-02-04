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

> module Feldspar.Compiler(scompile,icompile,IO) where

> import Feldspar.FrontEnd.AST 
> import Feldspar.BackEnd.AST
> import Feldspar.BackEnd.Pretty

> import Control.Monad.State

> type CompileMonad a = State (Int,[Var],[Var]) a

> newName :: CompileMonad String
> newName = do 
>  (i,ps,vs) <- get  
>  put (i+1,ps,vs) 
>  return $ "v" ++ (show i)

> addParam :: Var -> CompileMonad ()
> addParam p = do
>   (i,ps,vs) <- get 
>   put (i,p:ps,vs)

> addVar :: Var -> CompileMonad ()
> addVar v = do
>   (i,ps,vs) <- get 
>   put (i,ps,v:vs)

> runCompileMonad :: CompileMonad (Exp_C,Types,[Stmt]) ->
>                    Func
> runCompileMonad m = let 
>   ((exp_C,ty,stmts),(_,ps,vs)) = runState m (0,[],[])
>   in Func "test" (ps ++ [("*out",ty)])  
>      ([Declare t v |(v,t) <- vs] ++ 
>       stmts ++ [Assign "*out" exp_C])
 
> class Compilable t where
>  compile :: t -> CompileMonad (Exp_C,Types,[Stmt])
  
> instance GetType a => Compilable (Data a) where
>  compile e@(Var v)            = return $
>    (Var_C v      , getType e ,[])
>  compile e@(Lit_Int x)        = return $ 
>    (Num x        , getType e ,[])
>  compile e@(Lit_Bool True)    = return $ 
>    (Var_C "true" , getType e, []) 
>  compile e@(Lit_Bool False)   = return $ 
>    (Var_C "false", getType e, [])
>  compile e@(Add e_1 e_2)      = do
>    (e_c_1,_,st_1) <- compile e_1
>    (e_c_2,_,st_2) <- compile e_2
>    return $ (Infix e_c_1 "+"  e_c_2 
>             , getType e
>             , st_1 ++ st_2)
>  compile e@(Sub e_1 e_2)      = do
>    (e_c_1,_,st_1) <- compile e_1
>    (e_c_2,_,st_2) <- compile e_2
>    return $ (Infix e_c_1 "-"  e_c_2 
>             , getType e
>             , st_1 ++ st_2)
>  compile e@(Mul e_1 e_2)      = do
>    (e_c_1,_,st_1) <- compile e_1
>    (e_c_2,_,st_2) <- compile e_2
>    return $ (Infix e_c_1 "*"  e_c_2 
>             , getType e
>             , st_1 ++ st_2)
>  compile e@(Eq_Int e_1 e_2)   = do
>    (e_c_1,_,st_1) <- compile e_1
>    (e_c_2,_,st_2) <- compile e_2
>    return $ (Infix e_c_1 "==" e_c_2 
>             , getType e
>             , st_1 ++ st_2)
>  compile e@(LT_Int e_1 e_2)   = do
>    (e_c_1,_,st_1) <- compile e_1
>    (e_c_2,_,st_2) <- compile e_2
>    return $ (Infix e_c_1 "<"  e_c_2 
>             , getType e
>             , st_1 ++ st_2)
>  compile e@(And e_1 e_2)     = do
>    (e_c_1,_,st_1) <- compile e_1
>    (e_c_2,_,st_2) <- compile e_2
>    return $ (Infix e_c_1 "&&" e_c_2
>             , getType e
>             , st_1 ++ st_2)
>  compile e@(Not e_1)          = do 
>    (e_c_1,_,st_1) <- compile e_1
>    return $ (Unary "!"  e_c_1,getType e,st_1)
>  compile e@(If e_1 e_2 e_3) = do 
>    v <- newName  
>    let lv = (v,getType e)
>    addVar lv         
>    (e_c_1,_,st_1) <- compile e_1
>    (e_c_2,_,st_2) <- compile e_2
>    (e_c_3,_,st_3) <- compile e_3
>    return $ (Var_C v,getType e,st_1++[If_C e_c_1 
>                      (st_2 ++ [Assign v e_c_2])
>                      (st_3 ++ [Assign v e_c_3])])     

> instance (GetType a ,Compilable r) => 
>          Compilable (Data a -> r) where
>  compile f = do
>   v <- newName
>   let a = Var v
>   let r = f a
>   addParam (v,getType a)
>   compile  r

> class GetType a where
>   getType :: Data a -> Types

> instance GetType Int32 where
>   getType _ = TInt32 

> instance GetType Bool where
>   getType _ = TBool

> scompile :: Compilable a => a -> String 
> scompile = show . pretty . runCompileMonad . compile

> icompile :: Compilable a => a -> IO ()
> icompile = putStrLn . show . pretty . 
>            runCompileMonad . compile
 

 