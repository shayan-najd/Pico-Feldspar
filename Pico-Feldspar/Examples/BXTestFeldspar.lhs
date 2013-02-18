\begin{comment}

> {-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
> {-# LANGUAGE ExplicitForAll  #-}

\end{comment}
 
> module Example.BXTestFeldspar where
> 
> import Feldspar.AnnotationUtils (markAllF,markAll)
> import Feldspar.BX                (putAnn)
> import Examples.TestFeldspar      (incAbs)
> import Feldspar.Compiler.Compiler (scompile)

> -- the location of the generated C code
> c :: String
> c = "Examples/TestFeldspar.c"

> -- forward transformation from EDSL to C
> forward :: IO ()
> forward = writeFile c 
>           (scompile ((markAllF markAll) 
>                       incAbs))

> -- backward transformation from C to src-loc 
> backward :: IO ()
> backward = do  
>   cSrc <- readFile c
>   let r = putAnn False (markAllF markAll) 
>           incAbs cSrc
>   case r of    
>     Right locs -> putStrLn $ show locs   
>     Left er    -> putStrLn er
 