> {-# LANGUAGE TypeFamilies #-}

> module Annotations where

> class Inj t where
>  type Ann t 
>  inj  :: Ann t -> t -> t


> class Inj t => Annotatable t where
>  prj  :: t     -> Maybe ((Ann t,t))


> preserve :: (Annotatable t_Hi,Inj t_Lo, Ann t_Hi ~ Ann t_Lo) => 
>             t_Hi -> (t_Hi -> t_Lo) -> t_Lo
> preserve e_Hi f = case prj e_Hi of
>   Just (ann,e'_Hi) -> inj ann (f e'_Hi)  
>   Nothing          -> f e_Hi
