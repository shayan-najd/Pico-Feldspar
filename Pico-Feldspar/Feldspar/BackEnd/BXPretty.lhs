%format bx_PP
%format obs_X
%format toList_Doc
%format index_I
%%format eqShape_Doc = "(
\begin{comment}

> {-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

\end{comment}
This module contains the code needed to bidirectionalize the pretty-printing transformation.
\begin{newcode}

> {-# LANGUAGE Rank2Types  #-}
>  
> module Feldspar.BackEnd.BXPretty where
>
> import qualified Prelude
> import Prelude (Eq(..),Show(..),(.),Int,id,String
>  ,Bool(..),Functor(..),Read(..),Monad(..),Maybe(..)
>  ,Either(..),map,filter,($),fst,not,splitAt,read
>  ,tail,(+),length,(&&))

> import Text.PrettyPrint (Doc,int,text)
> import Control.Monad(unless)                   
> import Data.List (isPrefixOf,stripPrefix)
> import Data.Foldable (toList)
> import Data.Traversable(Traversable)
> import Data.Function (on)

> import BX (fromJust,fromList,index,assoc,validAssoc
>           ,union,lookupAll)                        
> import Feldspar.BackEnd.Pretty (Pretty(..))
 
\begin{comment}

> highlight :: forall a. a -> a
> highlight = id

\end{comment}

> -- lexical tokens
> data Token = 
>    Ann String  
>    -- the annotations (comments)
>  | Etc String
>    -- anything except annotations
>  deriving Show

> -- tokens are compared ignoring space
> -- and new-line characters
> instance Eq Token where
>  (Ann s) == (Ann s') = ((==) `on`
>    (filter (\x -> (x /= '\n') &&
>                   (x /=' ')))) s s'
>  (Etc s) == (Etc s') = ((==) `on`
>    (filter (\x -> (x /= '\n') &&
>                   (x /=' ')))) s s'
>  _       == _        = False

> -- checking if a token is an annotation 
> isAnn :: Token -> Bool
> isAnn (Ann _) = True
> isAnn _       = False

> -- lexical tokenizer
> tokenize :: String -> Maybe [Token]
> tokenize []     = Just []
> tokenize ('/':'*':' ': xs) = do
>  (before,after) <- splitBy " */" xs 
>  ts             <- tokenize after
>  return $ (Ann before) : ts
> tokenize (x:xs) = do  
>  ts <- tokenize xs  
>  return $ case ts of
>    []          -> Etc [x]    : ts
>    (Ann _):_   -> Etc [x]    : ts   
>    (Etc y):ts' -> Etc (x:y)  : ts'

> -- finding index of a string inside another string
> infixAt :: Eq a => [a] -> [a] -> Maybe Int
> infixAt needle haystack = infixAt' 0 needle haystack
>  where 
>   infixAt' _ _ []       = Nothing
>   infixAt' i n hs  
>    | n `isPrefixOf` hs  = Just i
>    | True               = infixAt' (i+1) n (tail hs)

> -- spliting a string by the given key string
> splitBy :: Eq a => [a] -> [a] -> Maybe ([a],[a])
> splitBy infx  s = do
>   i <- infixAt infx s
>   let (before,wafter) = splitAt i s
>   after <- stripPrefix infx wafter        
>   return (before,after)
 
> -- The format of the output string of
> -- pretty printing us to extract the annotations 
> -- by 1.tokenizing the string 2.extracting the 
> -- the comments 3.parsing the strings to the
> -- actual annotation values, hence the @Read@  
> -- constraint
> toList_Doc :: forall a. Read a => String -> [a]       
> toList_Doc d = [read s |  
>                 Ann s <- fromJust $ 
>                 tokenize d]          

> -- the shape of two output strings are 
> -- compared by ignoring the values in the
> -- comments
> eqShape_Doc :: String -> String -> Bool
> eqShape_Doc = (==) `on` 
>               (fmap (filter (not . isAnn)) 
>               . tokenize)

> -- since pretty printing uses type classes for 
> -- overloading, we are no longer able to use  
> -- our generic function (bff); we have to change
> -- the code slightly (as highlighted)
> bx_PP :: forall k. (Traversable k,Pretty (k Doc)) => 
>          (forall t. Pretty t => 
>            k t -> String) ->  
>          (forall a. (Read a,Eq a,Pretty a) =>
>            k a -> String ->
>            Either String (k a))
> bx_PP get s v = do 
>  let sL         = toList     s         
>  let vL         = toList_Doc v
>  let get_By_L :: forall a. (Read a ,Pretty a) => 
>                  [a] -> [a]
>      get_By_L x = highlight toList_Doc $ 
>                       get (fromList s x)        
>  unless (highlight eqShape_Doc (get s) v)       
>         $ Left "Modified view of wrong shape!"  
>  sL' <- bff_Pretty get_By_L sL vL
>  return $ fromList s sL'
 
> -- the version of bff working with lists and  
> -- pretty printing constraint; it does not
> -- check for validity of the mappings since
> -- the type @Doc@ is abstract and the exposed
> -- observer functions by the module, namely
> -- the functions @show@ and @render@ are not 
> -- used in our pretty printer
> bff_Pretty :: (forall a. (Read a,Pretty a) => 
>                [a] -> [a])-> 
>               (forall a. (Eq a,Pretty a) => 
>                [a] -> [a] -> Either String [a])
> bff_Pretty get s v = do  
>  -- Step 1  
>  let ms    = index s  
>  -- Step 2       
>  let is  = fst `map` ms   
>  let iv  = get is
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
>  -- check is removed
>  -- Step 6
>  return $ lookupAll is ms'  

> -- the put (backward) function that
> -- bidirectionalizes the pretty printer
> putPretty :: forall k a.
>              (Eq a, Read a, Traversable k
>              , Pretty (k Doc), Pretty a) =>
>              k a -> String -> Either String (k a)
> putPretty = bx_PP (show .  pretty . (fmap pretty))

> instance Pretty Doc  where
>  pretty = id
> instance Pretty Int  where
>  pretty = int
> instance Pretty Bool where
>  pretty = text . show
 
\end{newcode} 
 