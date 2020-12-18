module Model where

-- Exercise 1
-- I have chosen to lex all tokens apart from each other, since the lexer is not supposed to interpret things. 
-- It is only to create a clear structure. An identifier token does contain information, namely the String with the command.

data Token = TArrow | TDot | TComma | TGo | TTake |TMark | TNothing | TTurn | TCase | TOf | TEnd
			| TLeft | TRight | TFront | TSemicolon
			| TEmpty | TLambda | TDebris | TAsteroid | TBoundary | TUnderscore
			| TIdent String deriving Show
      


-- Exercise 2
-- I have chosen to create a program structure similar to the grammar. This way I was sure that I implemented a correct structure
-- In the rule of Cmds, there was also an epsilon option. This can be retrieved by parsing an emptylist. The same is for alts

data Program = Program [Rule] deriving Show
data Rule = Rule String Cmds deriving Show
data Cmds = Cmds [Cmd] deriving Show
data Cmd = GO | TAKE | MARK | NOTHING |
           TURN Dir |
           CASE Dir Alts |
           CMD String  deriving Show
data Dir = LEFT | RIGHT | FRONT  deriving Show
data Alts = Alts [Alt] deriving Show
data Alt = Alt Pat Cmds deriving Show
data Pat = EMPTY | LAMBDA | DEBRIS | ASTEROID | BOUNDARY | UNDERSCORE deriving (Show, Eq)
