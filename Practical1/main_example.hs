{-# language CPP #-}
#if __GLASGOW_HASKELL__ >= 804
import Prelude hiding ((*>), (<*), Monoid, mempty, foldMap, Foldable, (<>))
#elif __GLASGOW_HASKELL__ >= 710
import Prelude hiding ((*>), (<*), Monoid, mempty, foldMap, Foldable)
#endif

import ParseLib.Abstract

data Point2 = Point2 Int Int

instance Show Point2 where
    show(Point2 x y) = "(" ++ show x ++ "," ++ show y ++ ")2"

example1 :: String
example1 = "(10,10)"

parsePoint2 :: Parser Char Point2 
parsePoint2 = do
             b1 <- token "("
             x <- integer
             c1 <- token ","
             y <- integer
             b2 <- token ")"
             return (Point2 x y)

parsePoint2' :: Parser Char Point2 
parsePoint2' = (\a1 a2 a3 a4 a5 -> Point2 a2 a4) <$> token "(" <*> integer <*> token "," <*> integer <*>token ")"

parsePoint2'' :: Parser Char Point2 
parsePoint2'' = (\x y -> Point2 x y) <$> (token "(" *> integer) <*> (token "," *> integer <* token ")")
