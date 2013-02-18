\begin{comment}

> {-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
> {-# LANGUAGE ExplicitForAll  #-}

\end{comment}
In this module, the type classes |Functor|, |Foldable| and |Traversable| are derived for the high-level AST.
\begin{newcode}

> -- the code is omitted

\begin{comment}
 
> {-# LANGUAGE DeriveFunctor  #-}
> {-# LANGUAGE DeriveFoldable #-}
> {-# LANGUAGE DeriveTraversable #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE StandaloneDeriving #-}
>
> module Feldspar.FrontEnd.Derivings where
>
> import qualified Prelude as P
> import Prelude (Functor(..),($))
> import Data.Foldable(Foldable(..))
> import Data.Traversable(Traversable(..))
> import Control.Applicative(Applicative(..),(<$>),(<*>))
> import Data.Monoid(Monoid(..))

> import Feldspar.Types
> import Feldspar.FrontEnd.AST

> instance Functor (Data a) where
>   fmap _ (Var x)        = Var x    
>   fmap _ (Lit_Int i)    = Lit_Int i
>   fmap _ (Lit_Bool b)   = Lit_Bool b
>   fmap f (Not e)        = Not    (fmap f e)
>   fmap f (Add    e1 e2) = Add    (fmap f e1) (fmap f e2) 
>   fmap f (Sub    e1 e2) = Sub    (fmap f e1) (fmap f e2) 
>   fmap f (Mul    e1 e2) = Mul    (fmap f e1) (fmap f e2)  
>   fmap f (Eq_Int e1 e2) = Eq_Int (fmap f e1) (fmap f e2)  
>   fmap f (LT_Int e1 e2) = LT_Int (fmap f e1) (fmap f e2) 
>   fmap f (And    e1 e2) = And    (fmap f e1) (fmap f e2) 
>   fmap f (If  e1 e2 e3) = If     (fmap f e1) (fmap f e2) 
>                                  (fmap f e3)
>   fmap f (Ann ann e)    = Ann (f ann)        (fmap f e)      

> instance Foldable (Data a) where
>   foldMap _ (Var _)        = mempty    
>   foldMap _ (Lit_Int _)    = mempty
>   foldMap _ (Lit_Bool _)   = mempty
>   foldMap f (Not e)        = foldMap f e
>   foldMap f (Add    e1 e2) = foldMap f e1`mappend`foldMap f e2 
>   foldMap f (Sub    e1 e2) = foldMap f e1`mappend`foldMap f e2 
>   foldMap f (Mul    e1 e2) = foldMap f e1`mappend`foldMap f e2  
>   foldMap f (Eq_Int e1 e2) = foldMap f e1`mappend`foldMap f e2  
>   foldMap f (LT_Int e1 e2) = foldMap f e1`mappend`foldMap f e2 
>   foldMap f (And    e1 e2) = foldMap f e1`mappend`foldMap f e2 
>   foldMap f (If  e1 e2 e3) = foldMap f e1`mappend`foldMap f e2 
>                              `mappend` foldMap f e3
>   foldMap f (Ann ann e)    = f ann `mappend` foldMap f e

> instance Traversable (Data a) where
>   traverse _ (Var x)        = pure $ Var x    
>   traverse _ (Lit_Int i)    = pure $ Lit_Int i
>   traverse _ (Lit_Bool b)   = pure $ Lit_Bool b
>   traverse f (Not e)        = Not    <$> traverse f e
>   traverse f (Add    e1 e2) = Add    <$> traverse f e1 <*> traverse f e2 
>   traverse f (Sub    e1 e2) = Sub    <$> traverse f e1 <*> traverse f e2 
>   traverse f (Mul    e1 e2) = Mul    <$> traverse f e1 <*> traverse f e2  
>   traverse f (Eq_Int e1 e2) = Eq_Int <$> traverse f e1 <*> traverse f e2  
>   traverse f (LT_Int e1 e2) = LT_Int <$> traverse f e1 <*> traverse f e2 
>   traverse f (And    e1 e2) = And    <$> traverse f e1 <*> traverse f e2 
>   traverse f (If  e1 e2 e3) = If     <$> traverse f e1 <*> traverse f e2 
>                                      <*> traverse f e3
>   traverse f (Ann ann e)    = Ann <$> f ann <*> traverse f e

> deriving instance P.Eq (VarT a) 
> deriving instance P.Eq (SingTypes a)

\end{comment}
\end{newcode}