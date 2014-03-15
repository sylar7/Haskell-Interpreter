module Substitute where
import Base

data Exp  = Number    Int
          | Add       Exp Exp
          | Subtract  Exp Exp
          | Multiply  Exp Exp
          | Divide    Exp Exp
          | Variable  String        -- added
          deriving (Eq)

substitute1:: (String, Int) -> Exp -> Exp
substitute1 (var, val) exp = subst exp where
  subst (Number i)      = Number i
  subst (Add a b)       = Add (subst a) (subst b)
  subst (Subtract a b)  = Subtract (subst a) (subst b)
  subst (Multiply a b)  = Multiply (subst a) (subst b)
  subst (Divide a b)    = Divide (subst a) (subst b)
  subst (Variable name) = if var == name
                          then Number val
                          else Variable name

rename1:: (String, String) -> Exp -> Exp
rename1 (var, newvar) exp = rename exp where
  rename (Number i)       = Number i
  rename (Add a b)        = Add (rename a) (rename b)
  rename (Subtract a b)   = Subtract (rename a) (rename b)
  rename (Multiply a b)   = Multiply (rename a) (rename b)
  rename (Divide a b)     = Divide (rename a) (rename b)
  rename (Variable name)  = if var == name
                            then Variable newvar
                            else Variable name

type Env = [(String, Int)]

substitute :: Env -> Exp -> Exp
substitute env exp = subst exp where
  subst (Number i)      = Number i
  subst (Add a b)       = Add (subst a) (subst b)
  subst (Subtract a b)  = Subtract (subst a) (subst b)
  subst (Multiply a b)  = Multiply (subst a) (subst b)
  subst (Divide a b)    = Divide (subst a) (subst b)
  subst (Variable name) =
    case lookup name env of
      Just val -> Number val
      Nothing  -> Variable name

substitute1R env exp = foldr substitute1 exp env

instance Show Exp where
  show e = "[" ++ showExp 0 e ++ "]"

showExp level (Number i)      = show i
showExp level (Add a b)       = showBinary level 1 a " + " b
showExp level (Subtract a b)  = showBinary level 1 a " - " b
showExp level (Multiply a b)  = showBinary level 2 a "*" b
showExp level (Divide a b)    = showBinary level 2 a "/" b
showExp level (Variable x)    = x

showBinary outer inner a op b =
  if inner < outer then paren result else result
      where result = showExp inner a ++ op ++ showExp inner b
