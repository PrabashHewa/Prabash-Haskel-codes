module Main where

import Dates
import Data.List (sort, intercalate)
import Data.Maybe (isNothing)
import qualified Data.Map as Map

type EventName = String
type Place = String

data Event = Event {
    eventName :: EventName,
    place :: Place,
    date :: Date
} deriving (Eq, Ord, Show)

type Calendar = Map.Map EventName Event

parseDate :: String -> Maybe Date
parseDate dateStr =
    let (yyyy, rest1) = splitAt 4 dateStr
        (mm, rest2) = splitAt 2 (drop 1 rest1)
        dd = drop 1 rest2
    in makeMaybeDate (read yyyy) (read mm) (read dd)

addEvent :: Calendar -> EventName -> Place -> Date -> Calendar
addEvent calendar name place date = Map.insert name (Event name place date) calendar

findEvent :: Calendar -> EventName -> Maybe Event
findEvent = Map.lookup

eventsOnDate :: Calendar -> Date -> [Event]
eventsOnDate calendar date = sort [event | event <- Map.elems calendar, date == Dates.date event]

eventsAtPlace :: Calendar -> Place -> [Event]
eventsAtPlace calendar place = sort [event | event <- Map.elems calendar, place == Dates.place event]

respond :: Calendar -> String -> (String, Calendar)
respond calendar input =
    let tokens = words input
    in case tokens of
        ("Event" : nameRest) -> 
            let (name, "happens" : "at" : placeRest) = break (== "happens") nameRest
                (place, "on" : dateStr : []) = break (== "on") placeRest
                name' = unwords (stripQuotes name)
                place' = unwords (stripQuotes place)
                date = parseDate (stripQuotes dateStr)
            in case date of
                Just d -> ("Ok", addEvent calendar name' place' d)
                Nothing -> ("Bad date", calendar)

        ("Tell" : "me" : "about" : nameRest) ->
            let name' = unwords (stripQuotes nameRest)
            in case findEvent calendar name' of
                Just event -> (formatEvent event, calendar)
                Nothing -> ("I do not know of such event", calendar)

        ("What" : "happens" : "on" : dateStr : []) ->
            let date = parseDate (stripQuotes dateStr)
            in case date of
                Just d ->
                    let events = eventsOnDate calendar d
                    in if null events
                        then ("Nothing that I know of", calendar)
                        else (unlines (map formatEventOnDate events), calendar)
                Nothing -> ("Bad date", calendar)

        ("What" : "happens" : "at" : placeRest) ->
            let place' = unwords (stripQuotes placeRest)
                events = eventsAtPlace calendar place'
            in if null events
                then ("Nothing that I know of", calendar)
                else (unlines (map formatEventAtPlace events), calendar)

        ("Quit" : []) -> ("Bye", calendar)

        _ -> ("I do not understand that. I understand the following:\n*Event <name> happens at <place> on <date>\n*Tell me about <eventname>\n*What happens on <date>\n*What happens at <place>\n*Quit", calendar)

stripQuotes :: String -> String
stripQuotes = filter (/= '\'')

formatEvent :: Event -> String
formatEvent (Event name place date) = "Event " ++ name ++ " happens at " ++ place ++ " on " ++ show date

formatEventOnDate :: Event -> String
formatEventOnDate (Event name _ date) = "Event " ++ name ++ " happens on " ++ show date

formatEventAtPlace :: Event -> String
formatEventAtPlace (Event name place _) = "Event " ++ name ++ " happens at " ++ place

main :: IO ()
main = loop Map.empty

loop :: Calendar -> IO ()
loop calendar = do
    input <- getLine
    let (response, newCalendar) = respond calendar input
    putStrLn ("> " ++ input)
    putStrLn response
    if response == "Bye"
        then return ()
        else loop newCalendar


