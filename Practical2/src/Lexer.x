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
  \->                    {\s -> TArrow}
  go                    {\s -> Tgo}
  take                  {\s -> Ttake}
  mark                  {\s -> Tmark}
  nothing               {\s -> Tnothing}
  turn                  {\s -> Tturn}
  case                  {\s -> Tcase}
  of                    {\s -> Tof}
  end                   {\s -> Tend}
  left                  {\s -> Tleft}
  right                 {\s -> Tright}
  front                 {\s -> Tfront}
  \;                    {\s -> Tsemicolon}
  Empty                 {\s -> TEmpty}
  Lambda                {\s -> TLambda}
  Debris                {\s -> TDebris}
  Asteroid              {\s -> TAsteroid}
  Boundary              {\s -> TBoundary}
  \_                    {\s -> TUnderscore}
  $ident+               {\s -> TIdent s}

{
  
}