\ignore{
 
> {-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
 
}

> {-# LANGUAGE TypeSynonymInstances,FlexibleInstances,TypeFamilies #-}
  
> module Feldspar (module Feldspar.FrontEnd.Interface
>                 ,Data,AST.Int32,Num(..),String) where  
> import Feldspar.FrontEnd.Interface 
> import qualified Feldspar.FrontEnd.AST       as AST
> import qualified QuickAnnotate as QA
> import Feldspar.Annotations

> type Data a = AST.Data a String

> instance (Ann (Data a) ~ String) => QA.Annotatable (Data a) where
>  annotate loc d = inj loc d