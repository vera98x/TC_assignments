module Model where

-- Exercise 1
data Token = TArrow | TDot | TComma | TGo | TTake |TMark | TNothing | TTurn | TCase | TOf | TEnd
			| TLeft | TRight | TFront | TSemicolon
			| TEmpty | TLambda | TDebris | TAsteroid | TBoundary | TUnderscore
			| TIdent String deriving Show


-- Exercise 2
data Program = Program [Rule] deriving Show
data Rule = Rule String Cmds deriving Show
data Cmds = Cmds_ | Cmds Cmd [Cmd] deriving Show
data Cmd = GO | TAKE | MARK | NOTHING |
           TURN Dir |
           CASE Dir Alts |
           CMD String  deriving Show
data Dir = LEFT | RIGHT | FRONT  deriving Show
data Alts = Alts_ | Alts Alt [Alt] deriving Show
data Alt = Alt Pat Cmds deriving Show
data Pat = EMPTY | LAMBDA | DEBRIS | ASTEROID | BOUNDARY | UNDERSCORE deriving Show
