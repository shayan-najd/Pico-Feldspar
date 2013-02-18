{-# OPTIONS_GHC -F -pgmF qapp #-} 
module Examples.TestFeldspar2 where 
  
import qualified Prelude
import Feldspar 
import Feldspar.Compiler
 
incAbs :: Data Int32 -> Data Int32
incAbs a = condition (condition (a < 0) (true) (false)) 
                     (a+1) (a-1)
  
cCode :: IO () 
cCode = icompile incAbs 