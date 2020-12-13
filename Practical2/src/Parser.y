{
module Parser where

import Model
}

%name parseTokens
%tokentype { Token }
%error { parseError }

%token 
  string_   {TIdent $$}
  arrow     {TArrow}
  dot       {TDot}
  comma     {TComma}
  go        {TGo}
  take      {TTake}
  mark      {TMark}
  nothing   {TNothing}
  turn      {TTurn}
  case      {TCase}
  of        {TOf}
  end       {TEnd}
  left      {TLeft}
  right     {TRight}
  front     {TFront}
  semicolon {TSemicolon}
  empty     {TEmpty}
  lambda    {TLambda}
  debris    {TDebris}
  asteroid  {TAsteroid}
  boundary  {TBoundary}
  underscore {TUnderscore}

%%
Pprogram : Prules  {Program $1}

Prules : Prule {[$1]}
       | Prule Prules {$1 : $2}

Prule :  string_ arrow Pcmds dot {Rule $1 $3}

Pcmds : {- empty -}  { Cmds_ }
      | Pcmd PMultCmds {Cmds $1 $2}

PMultCmds : {- empty -} {[]}
          | comma Pcmd PMultCmds {$2 : $3}

Pcmd : go {GO}
     | take {TAKE}
     | mark {MARK}
     | nothing {NOTHING}
     | turn Pdir {TURN $2}
     | case Pdir of Palts end {CASE $2 $4}
     | string_ {CMD $1}

Pdir : left {LEFT}
     | right {RIGHT}
     | front {FRONT}

Palts : {- empty -}  { Alts_ }
      | Palt PMultAlts {Alts $1 $2}

PMultAlts : {- empty -} {[]}
          | semicolon Palt PMultAlts {$2 : $3}

Palt : Ppat arrow Pcmds {Alt $1 $3}

Ppat : empty {EMPTY}
     | lambda {LAMBDA}
     | debris {DEBRIS}
     | asteroid {ASTEROID}
     | boundary {BOUNDARY}
     | underscore {UNDERSCORE}

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}