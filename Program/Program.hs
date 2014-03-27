module Program where

import Prelude hiding (LT, GT, EQ, id)
import Data.Maybe

--BEGIN:Define data type -> Value:
data Value = IntV Int
           | BoolV Bool
           | VException 
           | ClosureV String Exp Env deriving (Eq, Show)
--END:Define data type -> Value


data ClosureExp = CExp Exp Env deriving (Eq, Show)
data Type = TInt | TBool | TFun Type Type deriving (Eq, Show)

--BEGIN:Define data type -> Exp:         
data Exp = Literal Value
         | Unary UnaryOp Exp
         | Binary BinaryOp Exp Exp
         | If Exp Exp Exp
         | Variable String
         | Declare String Exp Exp
         | Function String Exp
         | Call Exp Exp deriving (Eq, Show)
--END:Define data type -> Exp:

type Env = [(String, ClosureExp)]

evaluate :: Exp -> Env -> Value
evaluate (Literal v) env = v

evaluate (Unary op a) env = 
         unary op (evaluate a env)

evaluate (Binary op a b) env = 
         binary op (evaluate a env) (evaluate b env)

evaluate (If a b c) env = 
         let BoolV test = evaluate a env in
             if test then evaluate b env
                else evaluate c env

evaluate (Variable x) env = 
         case lookup x env of    
              Just (CExp name cenv) -> evaluate name cenv
              Nothing               -> fromJust Nothing  

evaluate (Declare x exp body) env = evaluate body newenv
         where newenv = (x,CExp exp env):env

evaluate (Function x body) env = 
         ClosureV x body env

evaluate (Call fun arg) env = 
         case evaluate arg env of
              VException                -> VException
              v                         -> case evaluate fun env of 
                                                ClosureV name body denv -> evaluate body ((name, CExp arg env) : denv)

--BEGIN:Define data type -> UnaryOp:
data UnaryOp = Not | Neg deriving (Eq, Show)

unary :: UnaryOp -> Value -> Value
unary Not (BoolV b) = BoolV (not b)
unary Neg (IntV  i) = IntV  (-i)

--END:Define data type -> UnaryOp:

--BEGIN:Define data type -> BinaryOp:
data BinaryOp = Add | Sub | Mul | Div | And | Or 
                | LT | GT | LE | GE | EQ 
                deriving (Eq, Show)

binary :: BinaryOp -> Value -> Value -> Value

binary Add (IntV a) (IntV b) = IntV (a + b)
binary Sub (IntV a) (IntV b) = IntV (a - b)
binary Mul (IntV a) (IntV b) = IntV (a * b)
binary Div (IntV a) (IntV b) 
       | (b == 0)  = VException
       | otherwise = IntV (a `div` b)
binary And (BoolV a) (BoolV b) = BoolV (a && b)
binary Or  (BoolV a) (BoolV b) = BoolV (a || b)
binary LT  (IntV  a) (IntV  b) = BoolV (a <  b)
binary GT  (IntV  a) (IntV  b) = BoolV (a >  b)
binary LE  (IntV  a) (IntV  b) = BoolV (a <= b)
binary GE  (IntV  a) (IntV  b) = BoolV (a >= b)
binary EQ  (IntV  a) (IntV  b) = BoolV (a == b)

--END:Define data type -> BinaryOp:

execute exp = evaluate exp []