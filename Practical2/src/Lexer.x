{
module Lexer where

import Model
}

%wrapper "basic"

$ident = [a-zA-Z0-9\+\-]		-- characters

tokens :-

  $white+				        ;
  "--".*				        ;
  \.					          { \s -> TDot }
  \,                    {\s -> TComma}
  \->                   {\s -> TArrow}
  go                    {\s -> TGo}
  take                  {\s -> TTake}
  mark                  {\s -> TMark}
  nothing               {\s -> TNothing}
  turn                  {\s -> TTurn}
  case                  {\s -> TCase}
  of                    {\s -> TOf}
  end                   {\s -> TEnd}
  left                  {\s -> TLeft}
  right                 {\s -> TRight}
  front                 {\s -> TFront}
  \;                    {\s -> TSemicolon}
  Empty                 {\s -> TEmpty}
  Lambda                {\s -> TLambda}
  Debris                {\s -> TDebris}
  Asteroid              {\s -> TAsteroid}
  Boundary              {\s -> TBoundary}
  \_                    {\s -> TUnderscore}
  $ident+               {\s -> TIdent s}

{
  
}