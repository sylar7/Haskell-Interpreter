module Simple where
import Base

data Exp = Number       Int
         | Add          Exp Exp
         | Subtract     Exp Exp
         | Multiply      Exp Exp
         | Divide       Exp Exp         

evaluate :: Exp -> Int
evaluate (Number n) = n
evaluate (Add      e1 e2) = evaluate e1 + evaluate e2 
evaluate (Subtract e1 e2) = evaluate e1 - evaluate e2
evaluate (Multiply e1 e2) = evaluate e1 * evaluate e2
evaluate (Divide   e1 e2) = evaluate e1 `div` evaluate e2

instance Show Exp where
         show e = "[" ++ showExp 0 e ++ "]"

showExp level (Number n)     = show n
showExp level (Add a b)      = showBinary level 1 a " + " b
showExp level (Subtract a b) = showBinary level 1 a " - " b
showExp level (Multiply a b) = showBinary level 1 a " * " b
showExp level (Divide a b)   = showBinary level 1 a " / " b

showBinary outer inner a op b =
           if inner < outer then paren result else result
              where result = showExp inner a ++ op ++ showExp inner b
