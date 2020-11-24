import Prelude hiding ((*>), (<*))
import ParseLib.Abstract


data Decalls = Decalls [Prop] deriving (Show)
data Prop = Prop Type Name0 Name1 Scale Position Euler FarCutoff NearCutoff RemoveTick deriving(Show)
data Type = Type String deriving(Show)
data Name0 = Name0 String deriving(Show)
data Name1 = Name1 String deriving(Show)
data Scale = Scale Int Int Int deriving(Show)
data Position = Position Int Int Int deriving(Show)
data Euler = Euler Int Int Int deriving(Show)
data FarCutoff = FarCutoff Int deriving(Show)
data NearCutoff = NearCutoff Int deriving(Show)
data RemoveTick = RemoveTick Int deriving(Show)


spaces :: Parser Char String
spaces = greedy (satisfy (== ' ')) <|> greedy (satisfy (== '\t'))

ending :: Parser Char String
ending = (token "'"  <|> succeed "") <* token "," <* token "\r" <* token "\n"

begin :: Parser Char Char
begin = spaces *> symbol '=' *> spaces *> (symbol '\'' <|> succeed ' ')

getVector :: Parser Char (Int, Int, Int)
getVector = (\x y z -> (x,y,z)) <$> (symbol '{' *> integer <* token ", ") <*> (integer <* token ", ") <*> (integer <* token ", " <* symbol '}')

data Token = TType String | TName0 String | TName1 String | TScale Int Int Int | TPosition Int Int Int | TEuler Int Int Int | 
             TFarCutoff Int | TNearCutoff Int | TRemoveTick Int deriving Show

test = "Decals = {\r\n    ['0'] = {\r\n        type = 'Normals',\r\n        name0 = '/env/common/decals/lowshore004_normals.dds',\r\n        name1 = '',\r\n        scale = { 56, 56, 56 },\r\n        position = { 834, 56, 733 },\r\n        euler = { -0, 6, 0 },\r\n        far_cutoff = 545454,\r\n        near_cutoff = 0,\r\n        remove_tick = 0, \r\n    }\r\n    }\r\n}"

scanWK :: Parser Char [Token]
scanWK = do
         _ <- identifier *> spaces *> symbol '=' *> spaces *> symbol '{' *> token "\r" *> token "\n"
         tokens <- many everyList
         return (concat tokens)

everyList = pack (spaces *> symbol '[') (anySymbol *> anySymbol <* anySymbol) (symbol ']') *> 
            pack (spaces *> symbol '=' *> spaces) (many options) (symbol '}' *> ending)

options :: Parser Char Token       
options = TType <$> (spaces *> token "type" *> pack begin identifier ending ) <|>
          TName0 <$> (spaces *> token "name0" *> pack begin identifier ending) <|>
          TName1 <$> (spaces *> token "name1" *> pack begin identifier ending) <|>
          (\(x,y,z) -> TScale x y z) <$> (spaces *> token "scale" *> pack begin getVector ending) <|>
          (\(x,y,z) -> TPosition x y z) <$> (spaces *> token "position" *> getVector <* ending) <|>
          (\(x,y,z) -> TEuler x y z) <$> (spaces *> token "euler" *> getVector <* ending) <|>
          TFarCutoff <$> (spaces *> token "far_cutoff" *> pack begin integer ending) <|>
          TNearCutoff <$> (spaces *> token "near_cutoff" *> pack begin integer ending) <|>
          TRemoveTick <$> (spaces *> token "remove_tick" *> pack begin integer ending)

