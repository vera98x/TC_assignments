{-# language CPP #-}
#if __GLASGOW_HASKELL__ >= 804
import Prelude hiding ((*>), (<*), Monoid, mempty, foldMap, Foldable, (<>))
#elif __GLASGOW_HASKELL__ >= 710
import Prelude hiding ((*>), (<*), Monoid, mempty, foldMap, Foldable)
#endif

import ParseLib.Abstract
import System.Environment

-- Starting Framework

-- | "Target" datatype for the DateTime parser, i.e, the parser should produce elements of this type.
data DateTime = DateTime { date :: Date
                         , time :: Time
                         , utc :: Bool }
    deriving (Eq, Ord)

data Date = Date { year  :: Year
                 , month :: Month
                 , day   :: Day }
    deriving (Eq, Ord)

newtype Year  = Year { unYear :: Int }  deriving (Eq, Ord)
newtype Month = Month { unMonth :: Int } deriving (Eq, Ord)
newtype Day   = Day { unDay :: Int } deriving (Eq, Ord)

data Time = Time { hour   :: Hour
                 , minute :: Minute
                 , second :: Second }
    deriving (Eq, Ord)

newtype Hour   = Hour { unHour :: Int } deriving (Eq, Ord)
newtype Minute = Minute { unMinute :: Int } deriving (Eq, Ord)
newtype Second = Second { unSecond :: Int } deriving (Eq, Ord)


-- | The main interaction function. Used for IO, do not edit.
data Result = SyntaxError | Invalid DateTime | Valid DateTime deriving (Eq, Ord)

instance Show DateTime where
    show = printDateTime

instance Show Date where
    show = printDate

instance Show Result where
    show SyntaxError = "date/time with wrong syntax"
    show (Invalid _) = "good syntax, but invalid date or time values"
    show (Valid x)   = "valid date: " ++ show x

main :: IO ()
main = mainDateTime

mainDateTime :: IO ()
mainDateTime = interact (printOutput . processCheck . processInput)
    where
        processInput = map (run parseDateTime) . lines
        processCheck = map (maybe SyntaxError (\x -> if checkDateTime x then Valid x else Invalid x))
        printOutput  = unlines . map show

mainCalendar :: IO ()
mainCalendar = do
    file:_ <- getArgs
    res <- readCalendar file
    putStrLn $ maybe "Calendar parsing error" (ppMonth (Year 2012) (Month 11)) res

-- Exercise 1
parseDateTime :: Parser Char DateTime
parseDateTime = do 
                d <- parseDate
                sep <- symbol 'T' 
                t <- parseTime   
                z <- symbol 'Z' <|> succeed ' '        
                return (DateTime d t (z == 'Z')) -- TODO: how to create bool?
parseHour :: Parser Char Hour
parseHour = (\i1 i2 -> Hour ((i1*10)+i2)) <$> integer <*> integer 

parseMinute :: Parser Char Minute
parseMinute = (\i1 i2 -> Minute ((i1*10)+i2)) <$> integer <*> integer 
parseSecond :: Parser Char Second
parseSecond = (\i1 i2 -> Second ((i1*10)+i2)) <$> integer <*> integer 
parseTime :: Parser Char Time
parseTime = Time <$> parseHour <*> parseMinute <*> parseSecond
parseYear :: Parser Char Year
parseYear = (\d1 d2 d3 d4 -> Year (d1*1000 + d2*100 + d3*10 + d4)) <$> integer <*> integer <*> integer <*> integer 
parseMonth :: Parser Char Month
parseMonth = (\i1 i2 -> Month ((i1*10)+i2)) <$> integer <*> integer 
parseDay :: Parser Char Day
parseDay = (\i1 i2 -> Day ((i1*10)+i2)) <$> integer <*> integer 
parseDate :: Parser Char Date
parseDate = Date <$> parseYear <*> parseMonth <*> parseDay 

-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
run p l = let x = filter (\(a,sl) -> length sl == 0)(parse p l) in case x of
          [] -> Nothing
          _  -> Just (fst (x!!0))

-- Exercise 3
printDateTime :: DateTime -> String
printDateTime dt = printDate (date dt) ++ "T" ++ printTime (time dt) ++ if (utc dt) then "Z" else ""

printDate :: Date -> String
printDate d =  printYear (year d) ++ printMonth (month d) ++ printDay (day d)

printYear :: Year -> String
printYear y = show (unYear y)
printMonth :: Month -> String
printMonth m = show (unMonth m)
printDay :: Day -> String
printDay d = show (unDay d)

printTime :: Time -> String
printTime t = printHour (hour t) ++ printMinute (minute t) ++ printSecond (second t)

printHour :: Hour -> String
printHour h = show (unHour h)
printMinute :: Minute -> String
printMinute m = show (unMinute m)
printSecond :: Second -> String
printSecond s = show (unSecond s)

-- Exercise 4
parsePrint s = fmap printDateTime $ run parseDateTime s

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime dt | (unMonth(month (date dt)) < 0 || unMonth(month (date dt)) > 13) = False
                 | (unYear(year (date dt)) < 0 ) = False
                 | (unMonth(month (date dt)) `elem` [1,3,5,7,8,10,12] && unDay(day (date dt)) > 31) = False
                 | (unMonth(month (date dt)) `elem` [4,6,9,11] && unDay(day (date dt)) > 30) = False
                 | (unHour(hour (time dt)) < 0 || unHour(hour (time dt)) >= 24) = False
                 | (unMinute(minute (time dt)) < 0 || unMinute(minute (time dt)) >= 60) = False
                 | (unMonth(month (date dt)) == 2) = if unYear(year (date dt)) `mod` 4 == 0 then unDay(day(date dt)) <= 29 else unDay(day (date dt)) <= 28
                 | otherwise = True

-- Exercise 6
data Calendar = Calendar [CalProp] [EventProp]
    deriving (Eq, Ord, Show)

data Event = Event EventProp  deriving (Eq, Ord, Show)
data EventProp = DTStamp DateTime | UID String | DTStart DateTime
                | DTEnd DateTime | Description String
                | Summary String | Location String
    deriving (Eq, Ord, Show)

data CalProp = Prodid String | Version deriving (Eq, Ord, Show)

-- Exercise 7
data Token = TokenEventStart | TokenEventEnd | TokenDtstamp | TokenUid |
             TokenDtstart | TokenDtend | TokenDescription | TokenSummary | TokenLocation |
             TokenCalendarStart | TokenCalendarEnd | TokenProdid | TokenVersion | TokenString String | Ignore
    deriving (Eq, Ord, Show)

scanCalendar :: Parser Char [Token]
scanCalendar = many scanCalendar2

scanCalendar2 :: Parser Char Token
scanCalendar2 = const TokenEventStart <$> token "BEGIN:VEVENT" <|> 
                const TokenEventEnd <$> token "END:VEVENT" <|> 
                const TokenDtstamp <$> token "DTSTAMP:" <|> 
                const TokenUid <$> token "UID:" <|> 
                const TokenDtstart <$> token "DTSTART:" <|> 
                const TokenDtend <$> token "DTEND:" <|> 
                const TokenDescription <$> token "DESCRIPTION:" <|> 
                const TokenSummary <$> token "SUMMARY:" <|> 
                const TokenLocation <$> token "LOCATION:" <|> 
                const TokenCalendarStart <$> token "BEGIN:VCALENDAR" <|> 
                const TokenCalendarEnd <$> token "END:VCALENDAR" <|> 
                const TokenProdid <$> token "PRODID:" <|> 
                const TokenVersion <$> token "VERSION:2.0" <|> 
                const Ignore <$> token "\r\n" <|> 
                TokenString <$> identifier

test = "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:hoi\r\nBEGIN:VEVENT"
test2 = "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nVERSION:2.0\r\nEND:VCALENDAR\r\n"

parseCalendar :: Parser Token Calendar
parseCalendar = do
                start <- satisfy (== TokenCalendarStart)
                _ <- satisfy (== Ignore)
                calls <- many caloptions
                events <- (satisfy (== TokenEventStart) *>  many eventOptions <* satisfy (== TokenEventEnd) ) <|> succeed []
                end <- satisfy (== TokenCalendarEnd) 
                _ <- satisfy (== Ignore)
                return (Calendar calls events)

isString = (\(TokenString x) -> True)

stringToDateTime :: Parser Token DateTime
stringToDateTime = do    
                  str <- satisfy (isString)
                  case str of
                    TokenString x -> let datetime = run parseDateTime x in
                                      case datetime of 
                                          Nothing -> failp 
                                          Just x  -> return x
                    _ -> failp

getString :: Parser Token String
getString = do
            str <- satisfy (isString)
            case str of
              TokenString x -> return x
              _ -> failp
eventOptions :: Parser Token EventProp
eventOptions =  (DTStamp <$> (satisfy (==TokenDtstamp) *> stringToDateTime <* satisfy (==Ignore))) <|>
                (UID <$> (satisfy (==TokenUid) *> getString <* satisfy (==Ignore))) <|>
                (DTStart <$> (satisfy (==TokenDtstart) *> stringToDateTime <* satisfy (==Ignore))) <|> 
                (DTEnd <$> (satisfy (==TokenDtend) *> stringToDateTime <* satisfy (==Ignore))) <|>  
                (Description <$> (satisfy (==TokenDescription) *> getString <* satisfy (==Ignore))) <|>  
                (Summary <$> (satisfy (==TokenSummary) *> getString <* satisfy (==Ignore))) <|>  
                (Location <$> (satisfy (==TokenLocation) *> getString <* satisfy (==Ignore)))  
caloptions :: Parser Token CalProp
caloptions = (Prodid <$> (satisfy (==TokenProdid) *> getString <* satisfy (==Ignore))) <|> 
             (const Version <$> (satisfy (==TokenVersion) <* satisfy (==Ignore))) 

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= run parseCalendar

-- Exercise 8
readCalendar :: FilePath -> IO (Maybe Calendar)
readCalendar = undefined

-- Exercise 9
-- DO NOT use a derived Show instance. Your printing style needs to be nicer than that :)
printCalendar :: Calendar -> String
printCalendar = undefined

-- Exercise 10
countEvents :: Calendar -> Int
countEvents = undefined

findEvents :: DateTime -> Calendar -> [Event]
findEvents = undefined

checkOverlapping :: Calendar -> Bool
checkOverlapping = undefined

timeSpent :: String -> Calendar -> Int
timeSpent = undefined

-- Exercise 11
ppMonth :: Year -> Month -> Calendar -> String
ppMonth = undefined

