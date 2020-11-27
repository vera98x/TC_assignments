import Prelude hiding ((*>), (<*), (<>))

import ParseLib.Abstract
import System.Environment
import System.IO 
import Data.List
import Data.List.Split
import Text.PrettyPrint.Boxes
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
printYear (Year y) | y < 10 = "0" ++ show y
                   | otherwise = show y
printMonth :: Month -> String
printMonth (Month m) | m < 10 = "0" ++ show m
                      | otherwise = show m
printDay :: Day -> String
printDay (Day d) | d < 10 = "0" ++ show d
                 | otherwise = show d

printTime :: Time -> String
printTime t = printHour (hour t) ++ printMinute (minute t) ++ printSecond (second t)

printHour :: Hour -> String
printHour (Hour h) | h < 10 = "0" ++ show h
                   | otherwise = show h
printMinute :: Minute -> String
printMinute (Minute m) | m < 10 = "0" ++ show m
                       | otherwise = show m
printSecond :: Second -> String
printSecond (Second s) | s < 10 = "0" ++ show s
                       | otherwise = show s

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
data Calendar = Calendar [CalProp] [Event]
    deriving (Eq, Ord, Show)

data Event = Event [EventProp]
    deriving (Eq, Ord, Show)

data EventProp = DTStamp DateTime | UID String | DTStart DateTime |DTEnd DateTime |Description String |
                 Summary String |Location String deriving (Eq, Ord, Show)

data CalProp = Prodid String | Version String deriving (Eq, Ord, Show)

-- Exercise 7
data Token = TokenEventStart | TokenEventEnd | TokenDtstamp | TokenUid |
             TokenDtstart | TokenDtend | TokenDescription | TokenSummary | TokenLocation |
             TokenCalendarStart | TokenCalendarEnd | TokenProdid | TokenVersion | TokenString String | Ignore
    deriving (Eq, Ord, Show)

scanCalendar :: Parser Char [Token]
scanCalendar = do list <- many scanCalendar2
                  return (concat list)

scanCalendar2 :: Parser Char [Token]
scanCalendar2 = const [TokenEventStart] <$> token "BEGIN:VEVENT" <<|> 
                const [TokenEventEnd] <$> token "END:VEVENT" <<|> 
                const [TokenDtstamp] <$> token "DTSTAMP:" <<|> 
                const [TokenUid] <$> token "UID:" <<|> 
                const [TokenDtstart] <$> token "DTSTART:" <<|> 
                const [TokenDtend] <$> token "DTEND:" <<|> 
                const [TokenDescription] <$> token "DESCRIPTION:" <<|> 
                const [TokenSummary] <$> token "SUMMARY:" <<|> 
                const [TokenLocation] <$> token "LOCATION:" <<|> 
                const [TokenCalendarStart] <$> token "BEGIN:VCALENDAR" <<|> 
                const [TokenCalendarEnd] <$> token "END:VCALENDAR" <<|> 
                const [TokenProdid] <$> token "PRODID:" <<|> 
                const [TokenVersion] <$> token "VERSION:" <<|> 
                const [Ignore] <$> token "\r\n" <<|> 
                (\s _ -> TokenString s : Ignore : []) <$> greedy(satisfy(/='\r')) <*> (token "\r\n") -- check directly for \r\n to make sure you dont go into infinite loop

test = "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nPRODID:hoi\r\nBEGIN:VEVENT"
test2 = "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nVERSION:2.0\r\nEND:VCALENDAR\r\n"
test9 = "BEGIN:VCALENDAR\r\nPRODID:prodid2\r\nVERSION:2.0\r\nBEGIN:VEVENT\r\nSUMMARY:Bastille Day Party\r\nUID:uid\r\nEND:VEVENT\r\nBEGIN:VEVENT\r\nSUMMARY:BastilleDayParty\r\nUID:uid\r\nEND:VEVENT\r\nEND:VCALENDAR\r\n"

parseCalendar :: Parser Token Calendar
parseCalendar = do
                start <- satisfy (== TokenCalendarStart)
                _ <- satisfy (== Ignore)
                calls <- many caloptions
                events <- many (Event<$>(satisfy (== TokenEventStart) *> satisfy (==Ignore) *>  many eventOptions <* satisfy (== TokenEventEnd) <* satisfy (==Ignore))) <|> succeed []
                end <- satisfy (== TokenCalendarEnd) 
                _ <- satisfy (== Ignore)
                return (Calendar calls events)

isString :: Token -> Bool
isString (TokenString y) = True
isString _               = False

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
             (Version <$> (satisfy (==TokenVersion) *> getString <* satisfy (==Ignore))) 

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= run parseCalendar

-- Exercise 8
readCalendar :: FilePath -> IO (Maybe Calendar)
readCalendar path = do
               handle <- openFile path ReadMode 
               hSetNewlineMode handle noNewlineTranslation 
               content <- hGetContents handle 
               return (recognizeCalendar content)

filepath = "examples/bastille.ics"

-- Exercise 9
-- DO NOT use a derived Show instance. Your printing style needs to be nicer than that :)
printCall :: [CalProp] -> String
printCall [] = ""
printCall ((Version x):xs) = "VERSION:" ++ x ++ "\r\n" ++ printCall xs
printCall ((Prodid x):xs) = "PRODID:" ++ x ++ "\r\n" ++ printCall xs

printEv :: [EventProp] -> String
printEv [] = ""
printEv ((DTStamp x):xs) = "DTSTAMP:" ++ printDateTime x ++ "\r\n" ++ printEv xs
printEv ((UID x):xs) = "UID:" ++ x ++ "\r\n" ++ printEv xs
printEv ((DTStart x):xs) = "DTSTART:" ++ printDateTime x ++ "\r\n" ++ printEv xs
printEv ((DTEnd x):xs) = "DTEND:" ++ printDateTime x ++ "\r\n" ++ printEv xs
printEv ((Description x):xs) = "DESCRIPTION" ++ x ++ "\r\n" ++ printEv xs
printEv ((Summary x):xs) = "SUMMARY:" ++ x ++ "\r\n" ++ printEv xs
printEv ((Location x):xs) = "LOCATIOM:" ++ x ++ "\r\n" ++ printEv xs

printEvent :: Event -> String
printEvent (Event xs) = printEv xs

printCalendar :: Calendar -> String
printCalendar (Calendar cal ev ) = "BEGIN:VCALENDAR\r\n" ++ printCall cal ++ "BEGIN:VEVENT\r\n" ++ unwords (map printEvent ev) ++ "END:VEVENT\r\n" ++ "END:VCALENDAR\r\n"

-- Exercise 10
countEvents :: Calendar -> Int
countEvents (Calendar cal ev) = length ev

isStartTime (DTStart x) = True
isStartTime _           = False

isEndTime (DTEnd x) = True
isEndTime _         = False

getEventProp :: [Event] -> [(EventProp, EventProp, Event)]
getEventProp ev = zip3 [x | (Event y) <- ev, x <- y, isStartTime x] [x | (Event y) <- ev, x <- y, isEndTime x] [x | x <- ev]

findEvents :: DateTime -> Calendar -> [Event]
findEvents dt (Calendar cal ev) = [x | (DTStart s, DTEnd e, x) <- (getEventProp ev), dt >= s && dt < e]

t = DateTime (Date (Year 1998) (Month 11) (Day 15)) (Time (Hour 00) (Minute 00) (Second 00)) True
t2 = DateTime (Date (Year 1998) (Month 11) (Day 15)) (Time (Hour 5) (Minute 00) (Second 00) ) True
t3 = DateTime (Date (Year 1998) (Month 11) (Day 15)) (Time (Hour 10) (Minute 00) (Second 00) ) True
t4 = DateTime (Date (Year 1998) (Month 11) (Day 15)) (Time (Hour 16) (Minute 00) (Second 00) ) True
cal = Calendar [] [ (Event [(DTStart t), (DTEnd t2), (Summary "dit moet een event zijn")] ), (Event [(DTStart t3), (DTEnd t4), (Summary "dit moet een event zijn")] ), (Event [(DTStart t), (DTEnd t4), (Summary "dit moet een event zijn")] ) ]

checkOverlapping :: Calendar -> Bool
checkOverlapping (Calendar cal ev) = checkOverlap (sort $ getEventProp ev)

checkOverlap :: [(EventProp, EventProp, Event)] -> Bool
checkOverlap [] = False
checkOverlap [x] = False
checkOverlap (((DTStart s), (DTEnd e), _) : x@((DTStart s1) ,(DTEnd e1),_) : xs) | s1 < e = True
                                               | otherwise = checkOverlap (x:xs)

timeSpent :: String -> Calendar -> Int
timeSpent summ (Calendar cal ev) = foldr (\(t1, t2) r -> timeDiff t1 t2 + r) 0 beginEnd
  where eventList = [e | e@(Event ep) <- ev, (Summary x) <- ep, x == summ]
        beginEnd = zip [x | (Event y) <- eventList, (DTStart x) <- y] [x | (Event y) <- eventList, (DTEnd x) <- y]


timeDiff :: DateTime -> DateTime -> Int
timeDiff (DateTime (Date (Year y) (Month mo)( Day d)) (Time (Hour h) (Minute mi) (Second s)) _) 
          (DateTime (Date (Year y1) (Month mo1)( Day d1)) (Time (Hour h1) (Minute mi1) (Second s1)) _) = (y1 - y) * 525948 + (mo1-mo)*43800 +(d1-d)*1440 +
                                                                                                         (h1-h)*60 + (mi1-mi)

-- Exercise 11
--TODO deze weghalen
endTest = do
    res <- readCalendar "examples/rooster_infotc.ics"
    putStrLn $ maybe "Calendar parsing error" (ppMonth (Year 2012) (Month 11)) res
---- 

ppMonth :: Year -> Month -> Calendar -> String
ppMonth y m c = intercalate ((replicate 105 '-') ++ "\n") $ map render $ map (foldr (<>) nullBox ) box1
 where box = updateCalendar (filterWithMonth y m $ getDates c) (flatBox (unMonth m))
       box1 = chunksOf 7 box

withHorizontal :: [[Box]] -> IO()
withHorizontal box = mapM_ printBox $ map (foldr (<>) nullBox ) box

withRender :: [[Box]] -> [String]
withRender box = map render $ map (foldr (<>) nullBox ) box

stringToScreen :: IO()
stringToScreen = mapM_ putStr (withRender b1)
  where b =  ((updateCalendar dates (flatBox 11)))         
        b1 = chunksOf 7 b

flatBox m = map (align left top 2 15) $ map (\x -> text (if x <= max then show x else "")) [1..max]
  where max | m `elem` [1,3,5,7,8,10,12] = 31
            | m `elem` [4,6,9,11] = 30
            | otherwise = 29

printbox :: [[String]] -> IO()
printbox box = mapM_ (mapM_ putStr) box

renderBox :: [Box] -> [[String]]
renderBox b = map (map render) box
  where box = chunksOf 7 b

testing moments = withHorizontal (chunksOf 7 (updateCalendar moments fbox))
  where fbox = flatBox 11

dates :: [(Int, (Hour, Minute), (Hour, Minute))]
dates = replicate 2 (15, (Hour 00, Minute 00), (Hour 05, Minute 30)) ++ [(16, (Hour 00, Minute 00), (Hour 05, Minute 30))]

insertOne :: (Int, (Hour,Minute), (Hour,Minute)) -> [Box] -> [Box]
insertOne (day, (hs, ms), (he, me)) b = begin ++ [box] ++ end
  where begin = take (day-1) b
        end = drop day b
        box = b!!(day-1) // (align left top 2 15 (text (printHour hs ++ ":" ++ printMinute ms ++ " - " ++ printHour he ++ ":" ++ printMinute me )))

updateCalendar :: [(Int, (Hour,Minute), (Hour,Minute))] -> [Box] -> [Box]
updateCalendar [] b = b
updateCalendar (x:xs) b = updateCalendar xs bNew
  where bNew = insertOne x b

filterWithMonth :: Year -> Month -> [(DateTime, DateTime)] -> [(Int, (Hour,Minute), (Hour,Minute))]
filterWithMonth (Year y) (Month m) dts = sort [(d,(h,mi),(h1,mi1)) | ((DateTime (Date (Year y1) (Month m1)( Day d)) (Time h mi b ) _ ), (DateTime  dat (Time h1 mi1 b1 ) _ )) <- dts, m == m1 && y == y1]

getDates :: Calendar -> [(DateTime, DateTime)]
getDates (Calendar cal ev) = zip [x | (Event y) <- ev, (DTStart x) <- y] [x | (Event y) <- ev, (DTEnd x) <- y]