import Data.List

data Year = MakeYear Integer deriving (Eq, Ord)
data Month = MakeMonth Integer deriving (Eq, Ord)
data Day = MakeDay Integer deriving (Eq, Ord)

data Date = Date { year :: Year, month :: Month, day :: Day } deriving (Eq, Ord)

instance Show Date where
   show (Date x y z) =  (show x) ++ "-" ++ (show y) ++ "-" ++ (show z)

instance Show Year where
   show (MakeYear x) = show x

instance Show Month where
   show (MakeMonth x) = toCorrectNumStr x

instance Show Day where
   show (MakeDay x) = toCorrectNumStr x

doCommand :: String -> IO [EventInfo] -> IO ()
doCommand input ioEvents = do
  events <- ioEvents --Now you can use events as [EventInfo]
  let rivi = words input
      jaetturivi = wordsJaa (=='\'') input

  if (length rivi == 0 || length jaetturivi == 0)
  then do
      putStrLn "I do not understand that. I understand the following:"
      putStrLn "*Event <name> happens at <place> on <date>"
      putStrLn "*Tell me about <eventname>"
      putStrLn "*What happens on <date>"
      putStrLn "*What happens at <place>"
      putStrLn "*Quit"
      loop $ return events

  else if ((jaetturivi !! 0 == "Event ") && (jaetturivi !! 2 == " happens at ") && (jaetturivi !! 4 == " on ") && (length jaetturivi == 6)) 
  then do
     let tapahtuma = jaetturivi !! 1
         paikka = jaetturivi !! 3
         pvList = pvmNroiksi (jaetturivi !! 5)
     
     if (length pvList == 3 && (correctDate (pvList !! 0) (pvList !! 1) (pvList !! 2)))
     then do
        let events2 = addEvent tapahtuma paikka pvList events
        putStrLn "ok"
        loop $ return events2
     else do
        putStrLn "Bad date"
        loop $ return events

  else if ((jaetturivi !! 0  == "Tell me about ") && (length jaetturivi == 2))
  then do
     let nimi = jaetturivi !! 1
     
     findEvent nimi events
     loop $ return events

  else if ((jaetturivi !! 0 == "What happens on ") && (length jaetturivi == 2))
  then do
      let pvmMJ = jaetturivi !! 1
          haetut = sortEvents(findEventsDate pvmMJ events)
      printEventsDate haetut
      loop $ return events

  else if ((jaetturivi !! 0 == "What happens at ") && (length jaetturivi == 2))
  then do
     let paikka = jaetturivi !! 1
         haetut = sortEvents (findEventsPlace paikka events)

     printEventsPlace haetut
     loop $ return events
  else do 
       putStrLn "I do not understand that. I understand the following:"
       putStrLn "*Event <name> happens at <place> on <date>"
       putStrLn "*Tell me about <eventname>"
       putStrLn "*What happens on <date>"
       putStrLn "*What happens at <place>"
       putStrLn "*Quit"
       loop $ return events
  
  
correctDate :: Integer -> Integer -> Integer -> Bool
correctDate 0 _ _ = False
correctDate y m d
 | (elem m [1,3,5,7,8,10,12]) && (elem d [1..31]) = True
 | (elem m [4,6,9,11]) && (elem d [1..30]) = True
 | (m==2) && (elem d [1..28]) = True
 | (leapYear (toYear y)) && (m==2) && (d==29) = True
 | otherwise = False

leapYear (MakeYear y)
 | mod y 400 == 0 = True
 | mod y 100 == 0 = False
 | mod y 4 == 0 = True
 | otherwise = False
 
makeDate :: Integer -> Integer -> Integer -> Date
makeDate y m d
 | correctDate y m d = Date { year = toYear y, month = toMonth m, day = toDay d }
 | otherwise = error "not correct combination of integers for year, month and day"

toYear :: Integer -> Year
toYear x
 | x == 0 = error "No year 0"
 | otherwise = MakeYear x

toMonth :: Integer -> Month
toMonth x
 | x < 1 = error "Minimum month number is 1"
 | x > 12 = error "Maximum month number is 12"
 | otherwise = MakeMonth x

toDay :: Integer -> Day
toDay x
 | x < 1 = error "Minimum day number is 1"
 | x > 31 = error "Maximum day number is 31"
 | otherwise = MakeDay x

pvmNroiksi :: String -> [Integer]
pvmNroiksi pvm = (read (take 4 pvm) :: Integer) : (read (pvm !! 5 : [pvm !! 6]) :: Integer): [(read (drop 8 pvm):: Integer)]

addEvent :: String -> String -> [Integer] -> [EventInfo] -> [EventInfo]
addEvent nimi paikka pvm kalenteri
   | length (deleteEvent nimi kalenteri) == (length kalenteri) = tapahtuma : kalenteri
   | otherwise = tapahtuma : (deleteEvent nimi kalenteri)
     where
       tapahtuma = EventInfo nimi paikka (makeDate (pvm !! 0) (pvm !! 1) (pvm !! 2))

deleteEvent :: String -> [EventInfo] -> [EventInfo]
deleteEvent _ [] = []
deleteEvent nimi [x] = if ((name x) == nimi) then [] else [x]
deleteEvent nimi (x:xs)
   | nimi == (name x) = xs
   | otherwise = (deleteEvent nimi xs) ++ [x]
   
findEvent :: String -> [EventInfo] -> IO ()
findEvent _ [] = putStrLn "I do not know of such event"
findEvent nimi (x:xs)
   | nimi == (name x) = putStrLn ("Event " ++ (name x) ++ " happens at " ++ (place x) ++ " on " ++ show (date x))
   | otherwise = findEvent nimi xs

toCorrectNumStr :: Integer -> String
toCorrectNumStr i
   | i >= 10 = show i
   | otherwise = "0" ++ (show i)

findEventsDate :: String -> [EventInfo] -> [EventInfo]
findEventsDate _ [] = []
findEventsDate pvm [x] = if (show(date x)) == pvm then [x] else []
findEventsDate pvm kalenteri = [x | x <- kalenteri, (show (date x) == pvm)]

findEventsPlace :: String -> [EventInfo] -> [EventInfo]
findEventsPlace _ [] = []
findEventsPlace paikka [x] =  if (place x) == paikka then [x] else []
findEventsPlace paikka kalenteri = [x | x <- kalenteri,  (place x) == paikka]

printEventsDate :: [EventInfo] -> IO ()
printEventsDate [] = putStrLn "Nothing that I know of"
printEventsDate [x] = putStrLn ("Event " ++ (name x) ++ " happens on " ++ (show (date x)))
printEventsDate (x:xs) = 
   do putStrLn ("Event " ++ (name x) ++ " happens on " ++ (show (date x)))
      printEventsDate xs

printEventsPlace :: [EventInfo] -> IO ()
printEventsPlace [] = putStrLn "Nothing that I know of"
printEventsPlace [x] = putStrLn ("Event " ++ (name x) ++ " happens at " ++ (place x))
printEventsPlace (x:xs) = 
   do putStrLn ("Event " ++ (name x) ++ " happens at " ++ (place x))
      printEventsPlace xs

printAllEvents :: [EventInfo] -> IO ()
printAllEvents [] = putStrLn "No events"
printAllEvents [x] = putStrLn ((name x) ++ " " ++ (place x) ++ " " ++ show (date x))
printAllEvents (x:xs) = 
   do putStrLn ((name x) ++ " " ++ (place x) ++ " " ++ show (date x))
      printAllEvents xs

sortEvents :: [EventInfo] -> [EventInfo]
sortEvents [] = []
sortEvents [x] = [x]
sortEvents xs = sortBy compareEvents xs

compareEvents a b
   | (name a) < (name b) = LT
   | (name a) > (name b) = GT
   | (name a) == (name b) = compare (show(date a)) (show(date b))

wordsJaa :: (Char -> Bool) -> String -> [String]
wordsJaa p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsJaa p s''
                            where (w, s'') = break p s'

-- Poista
data EventInfo = EventInfo { name :: String
                           , place :: String
                           , date :: Date
                           } deriving(Eq)

main = loop $ return []
 
loop :: IO [EventInfo] -> IO ()
loop ioEvents =
 do
 input <- getLine
 if input == "Quit"
   then putStrLn "bye"
   else doCommand input ioEvents
