{
module Lexer (lexer) where
}
%wrapper "basic"

tokens :-
  $white+				;

{
lexer = alexScanTokens
}
