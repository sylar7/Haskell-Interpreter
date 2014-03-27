module FirstClassFunctions where

import Prelude hiding (LT, GT, EQ, id)
import Data.Maybe

--BEGIN:A42
data Value = IntV  Int
           | BoolV Bool
           | ClosureV String Exp Env
  deriving (Eq, Show)
--END:A42

--BEGIN:Summ14
data Exp = Literal   Value
         | Unary     UnaryOp Exp
         | Binary    BinaryOp Exp Exp
         | If        Exp Exp Exp
         | Variable  String
         | Declare   String Exp Exp
         | Function  String Exp
         | Call      Exp Exp         -- changed
  deriving (Eq)
  
type Env = [(String, Value)]

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

evaluate (Variable x) env = fromJust (lookup x env)

evaluate (Declare x exp body) env = evaluate body newEnv
  where newEnv = (x, evaluate exp env) : env

evaluate (Function x body) env = 
  ClosureV x body env

evaluate (Call fun arg) env = 
  case evaluate fun env of 
     ClosureV name body denv -> 
       evaluate body ((name, evaluate arg env) : denv)
  

--END:Summ14 END:A47

execute exp = evaluate exp []

-- same as in IntBool.hs
data BinaryOp = Add | Sub | Mul | Div | And | Or
              | GT | LT | LE | GE | EQ
  deriving (Eq, Show)

data UnaryOp = Neg | Not
  deriving (Eq, Show)

unary Not (BoolV b) = BoolV (not b)
unary Neg (IntV i)  = IntV (-i)

binary Add (IntV a)  (IntV b)  = IntV (a + b)
binary Sub (IntV a)  (IntV b)  = IntV (a - b)
binary Mul (IntV a)  (IntV b)  = IntV (a * b)
binary Div (IntV a)  (IntV b)  = IntV (a `div` b)
binary And (BoolV a) (BoolV b) = BoolV (a && b)
binary Or  (BoolV a) (BoolV b) = BoolV (a || b)
binary LT  (IntV a)  (IntV b)  = BoolV (a < b)
binary LE  (IntV a)  (IntV b)  = BoolV (a <= b)
binary GE  (IntV a)  (IntV b)  = BoolV (a >= b)
binary GT  (IntV a)  (IntV b)  = BoolV (a > b)
binary EQ  a         b         = BoolV (a == b)

{-

| Function  String Exp      -- new

| ClosureV String Exp Env  -- new

--BEGIN:A44
evaluate (Function x body) env = ClosureV x body env     -- new
--END:A44

--BEGIN:A47
evaluate (Call fun arg) env = evaluate body newEnv    -- changed
  where ClosureV x body closeEnv = evaluate fun env
        newEnv = (x, evaluate arg env) : closeEnv

-}