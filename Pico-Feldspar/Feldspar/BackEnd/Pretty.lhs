%format ann           = "\colorbox{gray}{ann}"
\begin{comment}

> {-# OPTIONS_GHC -Wall #-}
> {-# LANGUAGE ExplicitForAll #-}
 
\end{comment}
This module contains the code for pretty-printing the low-level AST. It uses John Hughes's and Simon Peyton Jones's Pretty Printer Combinators \cite{HughesPJ}.

> {-# LANGUAGE FlexibleInstances #-}
>
> module Feldspar.BackEnd.Pretty where
>
> import qualified Prelude
> import Prelude (($),map,foldl1) 
> import Text.PrettyPrint (Doc,text,int,parens,semi,space
>                         ,comma,lbrace,rbrace,vcat,nest
>                         ,($+$),($$),(<>),(<+>))
> import qualified Data.List
> import Feldspar.BackEnd.AST
> import Feldspar.Types
 
> class Pretty a where
>  pretty :: a -> Doc
 
> instance Pretty ann  =>
>          Pretty (Exp_C ann) where
>  pretty (Var_C x) = text x   
>  pretty (Num i)   = int  i
>  pretty (Infix e_1 op e_2) = parens (pretty e_1 
>                                      <+> text op 
>                                      <+> pretty e_2)
>  pretty (Unary op e)       = parens (text op 
>                                      <+> pretty e)

\begin{newcode}

>  pretty (Ann_Exp_C ann e)  = text "/*" 
>                              <+> (pretty ann) <+> 
>                              text "*/"
>                              <+> pretty e 

\end{newcode}

> instance Pretty ann =>
>          Pretty (Stmt ann) where

>  pretty (If_C e_1 e_2 e_3) = text "if" 
>   <+> parens (pretty e_1) 
>   $+$ lbrace
>   $+$ nest 2 (vcat (map pretty e_2))
>   $+$ rbrace
>   $+$ text "else" 
>   $+$ lbrace
>   $+$ nest 2 (vcat (map pretty e_3))
>   $+$ rbrace

>  pretty (Assign v e)  = text v <+> text "=" 
>                         <+> pretty e <> semi

>  pretty (Declare t v) = pretty t <+> text v <> semi

\begin{newcode}

>  pretty (Ann_Stmt ann st) =  text "/*" 
>                              <+> (pretty ann) <+> 
>                              text "*/"
>                              $$ pretty st  

\end{newcode}

> instance Pretty ann =>
>          Pretty (Func ann) where

>  pretty (Func name vs body) = 
>   text "#include \"feldspar.h\""
>   $+$ text "void" <+> text name 
>   <+> parens (commaCat (map pretty vs) )
>   $+$ lbrace
>   $+$ nest 2 (vcat (map pretty body))
>   $+$ rbrace
 
> instance Pretty Var where
>  pretty (v,t) = pretty t <+> text v
 
> instance Pretty Types where
>  pretty Int32 = text "int32_t"
>  pretty Bool  = text "uint32_t"

> commaCat :: [Doc] -> Doc
> commaCat ds = foldl1 (<>) $ 
>               Data.List.intersperse (comma<>space) ds
 