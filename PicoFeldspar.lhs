\documentclass[11pt,a4paper]{report}
\input{includes/packagescommands}

%include colorcode.fmt

%format Forall (x) (f) = "\forall " x " .\; " f
%format Exist  (x) (f) = "\exists " x " .\; " f
%format forall x       = "\forall " x  
%format x_1         = x"_1"
%format x_2         = x"_2"
%format e_1         = e"_1"
%format e_2         = e"_2"
%format .           = "."
%format `o`         = "\circ"
%format =           = "="
%format ==          = "=="
%format ===         = "\equiv"
%format =/=         = "\not ="
%format /=          = "/="
%format <=>         = "\Leftrightarrow"
%format Highlight (x) = "\colorbox{gray}{\ensuremath{"x"}}"
%format ==    = "=="
%format e_c_1 = e"_{C1}"
%format e_c_2 = e"_{C2}"
%format e_c_3 = e"_{C3}"
%format e1 = e"_1"
%format e2 = e"_2"
%format e3 = e"_3"
%format e_3 = e"_3"
%format st_1 = st"_1"
%format st_2 = st"_2"
%format st_3 = st"_3"
%format Var_C = Var"_C"
%format exp_C = exp"_C"
%format . = "."
%format forall = "\forall"
%format Ann_Stmt 
%format Ann_Exp_C = "Ann_{Exp_C}"
 
\parskip 7.2pt 
\parindent 0pt
\hbadness 10000 
\barhs
\newenvironment{newcode}[0]
   {\definecolor{codecolor}{rgb}{0.5,0.5,0.5}}
   {\definecolor{codecolor}{rgb}{1,1,1}}
\definecolor{codecolor}{rgb}{1,1,1} 
\newcommand{\includecode}[2][c]{\lstinputlisting[caption=#2, escapechar=,]{#2}}

\begin{document} 
\begin{comment}

>

\end{comment}

\section*{Module Feldspar}
%include Pico-Feldspar/Feldspar.lhs

\section*{Module Annotations}
%include Pico-Feldspar/Annotations.lhs

\section*{Module BX}
%include Pico-Feldspar/BX.lhs

\section*{Module Feldspar.Compiler}
%include Pico-Feldspar/Feldspar/Compiler.lhs

\section*{Module Feldspar.Types}
%include Pico-Feldspar/Feldspar/Types.lhs

\section*{Module Feldspar.Annotations}
%include Pico-Feldspar/Feldspar/Annotations.lhs

\section*{Module Feldspar.AnnotationUtils}
%include Pico-Feldspar/Feldspar/AnnotationUtils.lhs

\section*{Module Feldspar.BX}
%include Pico-Feldspar/Feldspar/BX.lhs

\section*{Module Feldspar.FrontEnd.AST}
%include Pico-Feldspar/Feldspar/FrontEnd/AST.lhs

\section*{Module Feldspar.FrontEnd.Interface}
%include Pico-Feldspar/Feldspar/FrontEnd/Interface.lhs

\section*{Module Feldspar.FrontEnd.Derivings}
%include Pico-Feldspar/Feldspar/FrontEnd/Derivings.lhs

\section*{Module Feldspar.Compiler.Compiler}
%include Pico-Feldspar/Feldspar/Compiler/Compiler.lhs

\section*{Module Feldspar.Compiler.Compiler}
%include Pico-Feldspar/Feldspar/Compiler/Compiler.lhs

\section*{Module Feldspar.Compiler.BXCompiler}
%include Pico-Feldspar/Feldspar/Compiler/BXCompiler.lhs

\section*{Module Feldspar.BackEnd.AST}
%include Pico-Feldspar/Feldspar/BackEnd/AST.lhs

\section*{Module Feldspar.BackEnd.Pretty}
%include Pico-Feldspar/Feldspar/BackEnd/Pretty.lhs

\section*{Module Feldspar.BackEnd.Derivings}
%include Pico-Feldspar/Feldspar/BackEnd/Derivings.lhs

\section*{Module Feldspar.BackEnd.BXPretty}
%include Pico-Feldspar/Feldspar/BackEnd/BXPretty.lhs

\section*{Module Examples.TestFeldspar}
This module contains an example program written in the high-level language. 

< openBrace-# OPTIONS_GHC -F -pgmF qapp #-closeBrace 

%include Pico-Feldspar/Examples/TestFeldspar.lhs

\section*{C Code Examples.TestFeldspar}
\includecode{Pico-Feldspar/Examples/TestFeldspar.c}
%%include Pico-Feldspar/Examples/TestFeldspar.c

\section*{Module Examples.BXTestFeldspar}
%include Pico-Feldspar/Examples/BXTestFeldspar.lhs

\bibliographystyle{alpha}
\bibliography{includes/ref}
\addcontentsline{toc}{chapter}{\numberline{} Bibliography} % For pdf

\end{document}