\begin{comment}

> {-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
> {-# LANGUAGE ExplicitForAll  #-}

\end{comment}
The module containing the type classes and the functions to facilitate injecting, projecting and preserving the annotations.
\begin{newcode}

> {-# LANGUAGE TypeFamilies #-}
>
> module Annotations where
>
> import qualified Prelude
> import Prelude (Maybe(..))

> -- injecting annotations into data
> class Inj t where
>  type Ann t 
>  inj  :: Ann t -> t -> t

> -- projecting the stored annotations 
> class Inj t => Annotatable t where
>  prj  :: t     -> Maybe ((Ann t,t))

> -- preserving the annotations 
> preserve :: forall t_Hi t_Lo. 
>  (Annotatable t_Hi,Inj t_Lo
>  , Ann t_Hi ~ Ann t_Lo) => 
>  t_Hi -> (t_Hi -> t_Lo) -> t_Lo
> preserve e_Hi f = case prj e_Hi of
>   Just (ann,e'_Hi) -> inj ann (f e'_Hi)  
>   Nothing          -> f e_Hi

\end{newcode}