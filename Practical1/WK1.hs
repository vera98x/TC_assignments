{-# language CPP #-}
#if __GLASGOW_HASKELL__ >= 804
import Prelude hiding ((*>), (<*), Monoid, mempty, foldMap, Foldable, (<>))
#elif __GLASGOW_HASKELL__ >= 710
import Prelude hiding ((*>), (<*), Monoid, mempty, foldMap, Foldable)
#endif

import ParseLib.Abstract

data Point = Point Int Int deriving Show -- (10,11)
data Vector = Vector Int Int deriving Show -- <10,11>
data Entity = Entity Point Vector deriving Show -- Entity /r/n
                                  -- /t location: (10,10) /r/n
                                  -- /t speed: <10,11> /r/n

parsePoint:: Parser Char Point
parsePoint = (\x y -> Point x y) <$> (token "(" *> integer) <*> (token "," *> integer <* token ")")

parseVector :: Parser Char Vector
parseVector =  (\x y -> Vector x y) <$> (token "<" *> integer) <*> (token "," *> integer <* token ">")

parseEntity :: Parser Char Entity
parseEntity = (\x y -> Entity x y) <$> (identifier *> token "/r" *> token "/n" *> 
                                       token "/t" *> identifier *> token ":" *> parsePoint <* token "/r" <* token "/n") <*>
                                       (token "/t" *> identifier *> token ":" *> parseVector <* token "/r" <* token "/n") 

examplePoint = "(10,10)"
exampleVector = "<10,10>"
exampleEntity = "Entity/r/n/tlocation:(10,10)/r/n/tspeed:<10,11>/r/n"


test = (identifier) *> token "/" -- *> token "r" -- *> token "\n") -- *> token "\t" *> identifier *> token ":" *> parsePoint)