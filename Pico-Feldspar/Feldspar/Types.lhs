\begin{comment}

> {-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
> {-# LANGUAGE ExplicitForAll  #-}

\end{comment}
This module contains the declaration of the built-in types in Pico-Feldspar. It also includes the code defining singlton types and the utility functions for promotion and demotion of the built-in types. 

> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE ScopedTypeVariables #-}
>
> module Feldspar.Types where
>
> import qualified Prelude
> import Prelude (Eq (..))

> -- the built-in types 
> -- it is usually used in the promoted form
> data Types = Int32 | Bool
>  deriving Eq

> -- a GADT representation of a singleton type for
> -- the built-in types
> data SingTypes :: Types -> * where
>   SInt32 :: SingTypes Int32
>   SBool  :: SingTypes Bool

> -- overloaded function to demote singletons
> class SingT (n::Types) where
>   sing :: SingTypes n
> instance SingT Int32 where
>   sing = SInt32
> instance SingT Bool where
>   sing = SBool 

> -- coversion from singleton types to the original
> toTypes :: SingTypes n -> Types
> toTypes SInt32 = Int32
> toTypes SBool  = Bool

> -- overloaded function to demote singletons 
> -- to the original
> getType :: forall k n a. SingT n => k n a -> Types
> getType _ = toTypes (sing :: SingTypes n)

> -- an overloaded function to facilitate demotion
> -- using the type of the argument of a function
> getTypeF :: forall k n a r. SingT n => 
>              (k n a-> r) -> SingTypes n
> getTypeF _ = sing :: SingTypes n