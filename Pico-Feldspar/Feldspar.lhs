\begin{comment}

> {-# OPTIONS_GHC -Wall -fno-warn-orphans -fno-warn-incomplete-patterns #-}
> {-# LANGUAGE ExplicitForAll  #-}

\end{comment}
This module is used as a front-end to the Feldspar language. It re-exports from the internal modules.

> {-# LANGUAGE DataKinds #-}
>   
> module Feldspar (module Feldspar.FrontEnd.Interface                 
>                 ,Data,Int32,Num(..),String) where  
>
> import qualified Prelude
> import Prelude (String,Num(..))

> import Feldspar.FrontEnd.Interface
> import qualified Feldspar.FrontEnd.AST as AST
> import qualified Feldspar.Types as Types

\begin{newcode}

> import Feldspar.Annotations ()

\end{newcode}
\begin{comment} 

> type Highlight t = t
 
\end{comment}
 
> type Data a = AST.Data a (Highlight String) 
> type Int32  = Types.Int32
