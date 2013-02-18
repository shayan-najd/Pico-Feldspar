\begin{comment}

> {-# OPTIONS_GHC -Wall -fno-warn-orphans -fno-warn-incomplete-patterns #-}
> {-# LANGUAGE ExplicitForAll  #-}

\end{comment}
This module contains the code for our semantic bidirectionalization algorithm, described in the thesis.

> -- the code is omitted

\begin{comment}
\begin{newcode}

> {-# LANGUAGE DeriveFunctor #-}
> {-# LANGUAGE DeriveFoldable #-}
> {-# LANGUAGE DeriveTraversable #-}
> {-# LANGUAGE Rank2Types #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE FlexibleContexts #-}
>
> module BX where
>
> import Data.Foldable (Foldable(..),toList)
> import Data.Traversable (Traversable(..))
> import Control.Monad (unless,join)
> import qualified Prelude  
> import Prelude (String,Bool(..),Int,Eq(..),Either(..),Functor(..),Monad(..)
>                ,($),(!!),(||),not,fst,undefined,snd,map,lookup,length,and,zip
>                ,(+),(.),const,flip,mapM,Maybe(..))
> import qualified Control.Monad.State
> import Data.Function (on)
> import Data.List (unionBy) 

> fromJust :: Maybe a -> a
> fromJust (Just x) = x
> fromJust Nothing  = undefined
 
> index :: forall a. [a] -> [(Int, a)]
> index s         = zip [1..length s] s

> assoc :: forall a b. [a] ->[b] -> [(a,b)]
> assoc           = zip

> validAssoc :: forall a b. (Eq a,Eq b) => [(a, b)] -> Bool
> validAssoc mv   = and [ not (i == j) || x == y 
>                       | (i,x) <- mv, (j,y) <- mv]


> union :: forall a b. Eq a => [(a, b)] -> [(a, b)] -> [(a, b)]
> union           = unionBy ((==) `on` fst)

> lookupAll :: forall a b. Eq a => [a] -> [(a, b)] -> [b] 
> lookupAll is mp = map (fromJust . flip lookup mp) is

> data Nat = 
>    Zero 
>  | Succ Nat 

> infixr 5 :::
> data Vect :: Nat -> * -> * where
>   Nil   :: Vect Zero a
>   (:::) :: a -> Vect n a -> Vect (Succ n) a 

> instance Functor (Vect n) where
>   fmap _ Nil        = Nil
>   fmap f (x ::: xs) = f x ::: fmap f xs

> data SingNat :: Nat -> * where
>   SZero :: SingNat Zero
>   SSucc :: SingNat n -> SingNat (Succ n)
 
> class SingI (n::Nat) where
>   sing :: SingNat n

> instance SingI Zero where
>   sing = SZero

> instance SingI n => SingI (Succ n) where
>   sing = let n = (sing :: SingNat n) 
>          in SSucc n

> class (SingI (Size t)) => VectIso (t :: * -> *) where 
>   type Size t :: Nat
>   toVect   :: forall a. t a -> Vect (Size t) a
>   fromVect :: forall a. Vect (Size t) a -> t a  

> size     :: forall a t. (SingI (Size t),VectIso t) => 
>             t a -> SingNat (Size t) 
> size _  =  sing


> perm :: SingNat (Succ m) -> [(i,a)] -> 
>         [Vect (Succ m) (i,a)]
> perm  (SSucc SZero)     ms = (:::Nil) `map` ms
> perm  (SSucc (SSucc n)) ms = join 
>                           [((i,x):::) `map` 
>                            (perm (SSucc n) ms) 
>                           |(i,x) <- ms] 

> check_GBy :: forall t a x s. 
>  (VectIso t, Size t ~ Succ s, Eq x) => 
>  (t Int -> x) -> (t a -> x) -> [(Int,a)] -> Bool 
> check_GBy obs_I obs_X ms = let  
>    vs = perm (size (undefined :: t Int)) ms
>   in and [obs_I (fromVect  (fmap fst z))  == 
>           obs_X (fromVect (fmap snd z))
>          |z <- vs]  

> onG :: VectIso t => 
>        (t b -> c) -> (a -> b) -> (t a -> c)
> onG f f' = f . fromVect . (fmap f') . toVect 

> newtype Tuple_1 a = Tuple_1 a
> newtype Tuple_2 a = Tuple_2 (a,a)
> newtype Tuple_3 a = Tuple_3 (a,a,a)

> instance VectIso Tuple_1 where
>   type Size Tuple_1       = Succ Zero
>   toVect   (Tuple_1 x) = x ::: Nil      
>   fromVect (x ::: Nil) = Tuple_1 x
>
> instance VectIso Tuple_2 where
>   type Size Tuple_2    = Succ (Succ Zero)
>   toVect   (Tuple_2 (x_1 ,x_2))  = 
>             x_1 ::: x_2  ::: Nil      
>   fromVect (x_1 ::: x_2 ::: Nil) = 
>             Tuple_2 (x_1,x_2)
>
> instance VectIso Tuple_3 where
>   type Size Tuple_3    = Succ (Succ (Succ Zero))
>   toVect   (Tuple_3 (x_1, x_2, x_3)) = 
>             x_1 ::: x_2 ::: x_3 ::: Nil      
>   fromVect (x_1 ::: x_2 ::: x_3 ::: Nil) = 
>             Tuple_3 (x_1,x_2,x_3)

> fromList :: forall k a b. Traversable k => k a -> [b] -> k b
> fromList s lst =  let     
>   indices _ = do i  <- Control.Monad.State.get 
>                  Control.Monad.State.put (i+1) 
>                  return i   
>   si        = Control.Monad.State.evalState 
>               (Data.Traversable.mapM indices  s) 0
>   in fmap (lst!!) si 

> eqShape :: forall k a. (Eq (k ()),Foldable k,Functor k) => k a -> k a -> Bool
> eqShape = (==) `on` fmap (const ())
 
> bff_GUS_G :: forall x t s. 
>              (VectIso t,Eq x,Size t ~ Succ s) =>   
>              (forall a. (t a -> x) -> [a] -> [a])-> 
>              (forall a. Eq a => 
>              (t a -> x) -> [a] -> [a] -> Either String [a]) 
> bff_GUS_G get_By obs_X s v   
>  = do  
>  -- Step 1  
>  let ms    = index s 
>  let obs_I = onG obs_X (fromJust . (flip lookup ms))
>  -- Step 2       
>  let is  = fst `map` ms   
>  let iv  = get_By obs_I is
>  -- Step 3     
>  unless (length v == length iv) 
>         $ Left "Modified view of wrong length!"
>  let mv  = assoc iv v
>  -- Step 4       
>  unless (validAssoc mv)
>         $ Left "Inconsistent duplicated values!"
>  -- Step 5     
>  let ms' =  union mv ms
>  -- Step 5.1     
>  unless (check_GBy obs_I obs_X ms')
>         $ Left "Invalid modified view!"
>  -- Step 6
>  return $ lookupAll is ms'


> bff_GUS_G_Gen :: 
>  forall x k k' t s. 
>   (VectIso t,Functor k',Foldable k',Eq (k' ())
>   ,Size t ~ Succ s, Traversable k, Eq x) => 
>   (forall a. (t a -> x) -> k a -> k' a)-> 
>   (forall a. (Eq a) => 
>     (t a -> x) -> k a -> k' a ->
>     Either String (k a)) 
> bff_GUS_G_Gen get_By obs_X s v   
>  = do  
>  let sL     = toList s  
>  let vL     = toList v       
>  let get_By_L :: forall a. (t a->x) -> [a] -> [a]
>      get_By_L obs x = toList $ get_By obs (fromList s x)        
>  unless (eqShape (get_By obs_X s) v)       
>         $ Left "Modified view of wrong shape!"
>  sL' <- bff_GUS_G get_By_L obs_X sL vL 
>  return $ fromList s sL'

\end{newcode}
\end{comment}