\begin{comment}

> {-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
> {-# LANGUAGE ExplicitForAll  #-}

\end{comment}
This module is used as a front-end to the Feldspar compiler. It re-exports from the internal modules.

> module Feldspar.Compiler(icompile,scompile,IO) where
> import Feldspar.Compiler.Compiler