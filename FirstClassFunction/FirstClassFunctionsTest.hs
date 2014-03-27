import Base
import Prelude hiding (LT, GT, EQ)
import FirstClassFunctions
import FirstClassFunctionsParse

-- Javascript

p1 = parseExp ("var T = function(a) { function(b) { a } };"++
               "var F = function(a) { function(b) { b } };"++
               "var not = function(b) { b(F)(T) };"++
               "not(F)")

p2 = parseExp ("var absolute = function(x) {if (x > 0) x; else -x}; absolute(3)") 

p3 = parseExp ("var absolute = function(x) {if (x > 0) x; else -x}; absolute(-3)") 

p4 = parseExp ("var x = 2; var f = function(y) {x + y}; var x = 3; f(5)")   

-- Haskell

p5 = let x = 2 in let f y = x + y in let x = 3 in f 5
               
main = do
  tagged "FirstClassT1" (test "execute" execute p1)