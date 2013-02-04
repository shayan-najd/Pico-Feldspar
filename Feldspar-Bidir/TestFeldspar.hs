{-# OPTIONS_GHC -F -pgmF qapp #-} 
module TestFeldspar where
 
import qualified Prelude
import Feldspar 
import Feldspar.Compiler
  
inc :: Data Int32 -> Data Int32
inc x = x + 1 
 
dec :: Data Int32 -> Data Int32
dec x = x - 1 
  
incAbs :: Data Int32 -> Data Int32
incAbs a = condition (a < 0) (dec a) (inc a)
 
cCode :: IO () 
cCode = icompile incAbs 