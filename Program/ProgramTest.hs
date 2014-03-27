module ProgramTest where

import Base
import Prelude hiding (LT, GT, EQ)
import Program
import ProgramParser

-- Chinese PL

--Test Case 1:
--Define function Absolute 
--abs :: Int -> Int 
--abs x = if x > 0 then x else -x in Haskell
--Then Call the function

p0 = parseExp ("声明 绝对值 = 函数(实数) {如果 (实数 > 0) 那么 实数; 否则 -实数};绝对值(-10)")
e1 = evaluate p0 [] 

p1 = parseExp ("声明 T = 函数(a) { 函数(b) { a } };"++
               "声明 F = 函数(a) { 函数(b) { b } };"++
               "声明 not = 函数(b) { b(F)(T) };"++
               "not(F)")

p2 = parseExp ("声明 absolute = 函数(x) {如果 (x > 0) 那么 x; 否则 -x}; absolute(3)") 

p3 = parseExp ("声明 absolute = 函数(x) {如果 (x > 0) 那么 x; 否则 -x}; absolute(-3)") 

p4 = parseExp ("声明 x = 2; 声明 f = 函数(y) {x + y}; 声明 x = 3; f(5)")   

p5 = parseExp ("声明 x = 3 / 0; x")

p6 = parseExp ("声明 x = 3 / 0; 7")

p7 = parseExp ("(函数(x) {7})(8/0)")

-- Haskell

h1 = let x = 2 in let f y = x + y in let x = 3 in f 5

-- Abstract syntax version of some programs with types (for question 2):
{-

a2 = Declare "absolute" (Function ("x", TInt) (If (Binary GT (Variable "x") (Literal (IntV 0))) (Variable "x") (Unary Neg (Variable "x")))) (Call (Variable "absolute") (Literal (IntV 3)))

a3 = Declare "absolute" (Function ("x", TInt) (If (Binary GT (Variable "x") (Literal (IntV 0))) (Variable "x") (Unary Neg (Variable "x")))) (Call (Variable "absolute") (Unary Neg (Literal (IntV 3))))
           
a5 = Declare "x" (Binary Div (Literal (IntV 3)) (Literal (IntV 0))) (Variable "x")
    
a6 = Declare "x" (Binary Div (Literal (IntV 3)) (Literal (IntV 0))) (Literal (IntV 7))

a7 = Call (Function ("x",TInt) (Literal (IntV 7))) (Binary Div (Literal (IntV 8)) (Literal (IntV 0)))

a8 = Function ("x",TInt) (Literal (IntV 7))
-}
               
main = do
   tagged "FirstClassT1" (test "execute" execute p0)