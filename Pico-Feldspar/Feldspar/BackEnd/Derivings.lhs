\begin{comment}

> {-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

\end{comment}
In this module, the type classes |Eq|,|Functor|,|Foldable| and |Traversable| is derived for the AST of the low-level language.
\begin{newcode}

> -- the code is omitted

\begin{comment}

> {-# LANGUAGE DeriveFunctor  #-}
> {-# LANGUAGE DeriveFoldable #-}
> {-# LANGUAGE DeriveTraversable #-}
> {-# LANGUAGE StandaloneDeriving #-}
> {-# LANGUAGE FlexibleInstances #-}
>
> module Feldspar.BackEnd.Derivings where
>
> import qualified Prelude as P
> import Prelude (Eq(..),Functor(..))
> import Data.Foldable(Foldable(..))
> import Data.Traversable(Traversable(..))
 
> import Feldspar.BackEnd.AST

> deriving instance Foldable Exp_C
> deriving instance Foldable Stmt 
> deriving instance Foldable Func
> deriving instance Functor Exp_C
> deriving instance Functor Stmt 
> deriving instance Functor Func
> deriving instance Traversable Exp_C
> deriving instance Traversable Stmt 
> deriving instance Traversable Func
> deriving instance Eq ann => Eq (Exp_C ann)
> deriving instance Eq ann => Eq (Stmt ann)   
> deriving instance Eq ann => Eq (Func ann) 

\end{comment}
\end{newcode}