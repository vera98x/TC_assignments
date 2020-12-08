module Model where

-- Exercise 1
data Token = TArrow | TDot | TComma | Tgo | Ttake |Tmark | Tnothing | Tturn | Tcase | Tof | Tend
			| Tleft | Tright | Tfront | Tsemicolon
			| TEmpty | TLambda | TDebris | TAsteroid | TBoundary | TUnderscore
			| TIdent String


-- Exercise 2
data Program = Program [Rule]
data Rule = Rule String Cmds
data Cmds = Cmds_ | Cmds Cmd [Cmd]
data Cmd = GO | TAKE | MARK | NOTHING |
           TURN Dir |
           CASE Dir Alts |
           CMD String
data Dir = LEFT | RIGHT | FRONT
data Alts = Alts_ | Alts Alt [Alt]
data Alt = Alt Pat Cmds
data Pat = Empty | Lambda | Debris | Asteroid | Boundary | Underscore
