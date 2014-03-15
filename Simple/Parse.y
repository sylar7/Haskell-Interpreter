{
module Main where
import Data.Char
import Lexer
}


%name calc
%tokentype { Token }
%error { parseError }

%token 
      let             { TokenLet }
      in              { TokenIn }
      digits          { Digits $$ }
      int             { TokenInt $$ }
      var             { TokenVar $$ }
      '='             { TokenEq }
      '+'             { TokenPlus }
      '-'             { TokenMinus }
      '*'             { TokenTimes }
      '/'             { TokenDiv }
      '('             { TokenOB }
      ')'             { TokenCB }

%%

Exp : digits        { Number $1 }
    | '-' digits    { Number (- $2) }
    | Exp '+' Exp   { Add $1 $3 }
    | Exp '-' Exp   { Subtract $1 $3 }
    | Exp '*' Exp   { Multiply $1 $3 }
    | Exp '/' Exp   { Divide $1 $3 }
    | '(' Exp ')'   { $2 }


{

symbols = ["+", "-", "*", "/", "(", ")"]
keywords = []
parseExp str = parser (lexer symbols keywords str)

parseInput = do
  input <- getContents
  print (parseExp input)

}
