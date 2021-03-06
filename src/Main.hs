module Main where

import Control.Applicative
import Control.Arrow
import Data.Function
import Data.List
import Data.WhatsApp
import System.Environment
import Text.Parsec hiding ((<|>), many)
import Text.Tabular
import Text.Tabular.AsciiArt
import Data.Time.Clock
import Data.Time.Calendar

date :: IO (Integer,Int,Int) -- :: (year,month,day)
date = getCurrentTime >>= return . toGregorian . utctDay

data Match  = Match {matchdate :: Date, local :: Int, away :: Int} deriving (Eq, Ord, Show)
data Result = L | X | A deriving (Eq, Ord, Enum, Show)

isAResultTriplet :: (Date,Either a (Int,Int)) -> Bool
isAResultTriplet (_, Right _) = True
isAResultTriplet _ = False

processChat :: [WhatsAppMessage] -> [Match]
processChat (msgs) = map (\(d, Right (l,a)) -> Match d l a)
                         (filter isAResultTriplet $ zip dates goals')
    where
        dates  = map _date msgs
        goals' = map (runParser resultParser () "" . _message) msgs

resultParser :: Parser (Int, Int)
resultParser =  do
    spaces
    l <- numberParser
    spaces *> string "-" *> spaces
    a <- numberParser
    return (l,a)

result  :: Match -> Result
result  (Match _ l a) | l > a     = L
                      | l < a     = A
                      | otherwise = X

clamp :: (Ord a) => a -> a -> a -> a
clamp mi ma x | x < mi    = mi
              | x > ma    = ma
              | otherwise = x

winner :: [Result] -> Result
winner ms = toEnum (1 + clamp (-1) 1 s)
    where
        (i, _, ii) = summarizeResults ms
        s = ii - i

totalGoals :: [Match] -> (Int, Int)
totalGoals = foldr1 (\(a,b) (c,d) -> (a+c, b + d)) . map goals

dot :: (c -> d) -> (a -> b -> c) -> a -> b -> d
dot = (.).(.)

lf :: (a -> Bool) -> [a] -> Int
lf = length `dot` filter

summarizeResults :: [Result] -> (Int, Int, Int)
summarizeResults results = (ones, xs,  twos)
    where
        ones = lf (== L) results
        twos = lf (== A) results
        xs   = lf (== X) results

sameRound :: Date -> Date -> Bool
sameRound (Date y1 m1 d1 h1 _) (Date y2 m2 d2 h2 _) = (abs $ (f y1 m1 d1 h1) - (f y2 m2 d2 h2)) < 6
	where f y m d h = 365 * 24 * y + 30 * 24 * m + d * 24 + h

summarizeResultsByDay :: [Match] -> (Int, Int, Int)
summarizeResultsByDay ms = summarizeResults days
    where
        resultsGroupedByDate = map (map result) $ groupBy (sameRound `on` matchdate) ms
        days = map winner resultsGroupedByDate

goals :: Match -> (Int, Int)
goals = local &&& away

processResult :: [Match] -> Table String String Int
processResult matches = Table
    (Group SingleLine [Header "Goals", Header "Wins", Header "Rounds"])
    (Group SingleLine [Header "Local", Header "Away"])
    [ [golesLocal,    golesAway]
    , [matchesLocal, matchesAway]
    , [diasLocal,     diasAway]
    ]
    where
        (golesLocal, golesAway)          = totalGoals matches
        (matchesLocal, _, matchesAway)   = summarizeResults $ map result matches
        (diasLocal, _, diasAway)         = summarizeResultsByDay matches

mostCommon :: (Eq a, Ord a) => [a] -> a
mostCommon = head . maximumBy compare . group

main :: IO ()
main = do
    filename:_ <- getArgs
    matches    <- parseWhatsAppChatFromFile filename
    (y,_,_)    <- date
    putStrLn ""
    case matches of
        Right x -> putStrLn $ render id id show $ processResult $ processChat $ (filter ((== fromIntegral y) . _year . _date )) x
        _       -> print matches
    putStrLn ""
