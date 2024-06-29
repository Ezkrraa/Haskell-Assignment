import Control.Concurrent
import Control.Monad (replicateM)
import Data.List
import Data.Time
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Time.Clock.System

-- import System.Random (randomRIO)

-- INFADP01-D Practicumopdracht 3
-- W. Oele
-- 19 april 2024
-- 1
-- Inleiding
-- In deze opdracht hou je je met verschillende zaken bezig:
--  Werken met bestaande bibliotheken.
--  Partieel parameteriseren.
--  Oefenen met diverse ingebouwde functies zoals filter, map, de fold
-- functies, etc.

-- Opdracht 1a : partieel parameteriseren
-- Schrijf de functie:
-- differentieer::(Double->Double)->Double->Double->Double
-- differentieer f p x
-- Deze functie differentieert de functie f numeriek in punt x met een precisie p.
--  Hint 1: bestudeer het differentiequotient.
--  Hint 2: kies voor p een klein getal, maar niet te klein, bijv. 1
-- 10.000

differentieer :: (Double -> Double) -> Double -> Double -> Double
differentieer f p x = (f (x + p) - f x) / p

testdifferentieer :: IO ()
testdifferentieer = do
  let f x = x ^ 2
  print $ differentieer f 1 2

-- Opdracht 1b : partieel parameteriseren
-- Schrijf de functie:
-- integreer::(Double->Double)->Double->Double->Double->Double
-- integreer f a b p
-- Deze functie integreert de functie f op het interval a, b met een precisie p.
--  Hint 1: bestudeer wat men verstaat onder Riemannintegratie/de Rieman-
-- nsom.
--  Hint 2: kies voor p een klein getal, maar niet te klein, bijv. 1
-- 10.000
-- 2
integreer :: (Double -> Double) -> Double -> Double -> Double -> Double
integreer f a b p = sum [f (a + i * p) * p | i <- [0 .. (b - a) / p]]

testintegreer :: IO ()
testintegreer = do
  let f x = x ^ 2
  print $ integreer f 0 2 1

-- Opdracht 2: werken met bibliotheken
-- Zoek op internet naar de module Data.List. Deze is te vinden in de hackage
-- database. Importeer deze module in je programma. Functies kunnen ook worden
-- opgezocht in de hackage database m.b.v. een daarvoor geschikte zoekmachine
-- zoals http://www.haskell.org/hoogle/.
-- Schrijf de functie:
dubbelen :: (Eq a) => [a] -> [a]
dubbelen s = nub [x | x <- s, length (filter (== x) s) > 1]

-- Deze functie heeft een lijst als invoer en levert als uitvoer een lijst met uitsluitend
-- die elementen die meer dan een keer voorkomen in de lijst. Voorbeeld:
-- ghci>dubbelen "aabbbcdeeeef"
-- ghci>"abe"
testdubbelen :: IO ()
testdubbelen = do
  print $ dubbelen "aabbbcdeeeef"

-- Gebruik de functies uit Data.List om bovenstaande functie op te bouwen.

-- Opdracht 3: hogere orde functies
-- Het pokerspel is een spel dat gespeeld wordt met 5 dobbelstenen. Na het werpen
-- van 5 dobbelstenen zijn de volgende combinaties winnend:
-- Poker 5 stenen met gelijke ogen
-- Four of a kind 4 stenen met gelijke ogen
-- Three of a kind 3 stenen met gelijke ogen
-- Full house 3 stenen met gelijke ogen en nog eens twee stenen met gelijke
-- ogen,bijv. 44554
-- Two pair Twee paar met gelijke ogen, bijv. 35453
-- One pair Een paar met gelijke ogen, bijv. 12344
-- Straight De reeks 1,2,3,4,5 of 2,3,4,5,6
-- Bust De overgebleven worpen
-- Schrijf een aantal functies, waarmee je de kansen kunt berekenen op boven-
-- staande uitkomsten. Gebruik naar eigen inzicht functies zoals
-- map, filter, any, all, foldr, foldl, etc.

-- Defenitie variabele
newtype Dobbelsteen = Dobbelsteen Int deriving (Show, Eq)

type Hand = [Dobbelsteen]

-- Help functies
newDobbelsteen :: Int -> Dobbelsteen
newDobbelsteen = Dobbelsteen

-- >>Get current time in picoseconds
currentTimeInPicoseconds :: IO Integer
currentTimeInPicoseconds = do
  currentTime <- getPOSIXTime
  let diffTime = picosecondsToDiffTime (round (currentTime * 1e7))
  return $ diffTimeToPicoseconds diffTime

-- >>Convert picoseconds to a number between 1 and 6
randomNumber1to6 :: IO Int
randomNumber1to6 = do
  pico <- currentTimeInPicoseconds
  let randomNum = fromIntegral (pico `mod` 6 + 1)
  return randomNum

-- >>Generate a new hand with five dice rolls
newHand :: IO Hand
newHand = do
  replications <-
    replicateM
      5
      ( do
          num <- randomNumber1to6
          threadDelay 2753
          return num
      )
  return $ map newDobbelsteen replications

-- Pattronen check
-- >>Functie om de waarden uit een hand te extraheren
handValues :: Hand -> [Int]
handValues = map (\(Dobbelsteen v) -> v)

-- >>Frequentie van elk element in de lijst
frequencies :: (Ord a) => [a] -> [(a, Int)]
frequencies = map (\l -> (head l, length l)) . group . sort

-- >>Check functies voor de combinaties
isPoker :: Hand -> Bool
isPoker hand = any (\(_, count) -> count == 5) (frequencies $ handValues hand)

isFourOfAKind :: Hand -> Bool
isFourOfAKind hand = any (\(_, count) -> count == 4) (frequencies $ handValues hand)

isThreeOfAKind :: Hand -> Bool
isThreeOfAKind hand = any (\(_, count) -> count == 3) (frequencies $ handValues hand)

isFullHouse :: Hand -> Bool
isFullHouse hand =
  let freq = frequencies $ handValues hand
   in (length freq == 2) && any (\(_, count) -> count == 3) freq

isTwoPair :: Hand -> Bool
isTwoPair hand =
  let freq = frequencies $ handValues hand
   in length (filter (\(_, count) -> count == 2) freq) == 2

isOnePair :: Hand -> Bool
isOnePair hand = any (\(_, count) -> count == 2) (frequencies $ handValues hand)

isStraight :: Hand -> Bool
isStraight hand =
  let values = sort $ handValues hand
   in values == [1, 2, 3, 4, 5] || values == [2, 3, 4, 5, 6]

-- Combinatie patroon checkers
welkeCombinatie :: Hand -> String
welkeCombinatie hand
  | isPoker hand = "Poker"
  | isFourOfAKind hand = "Four of a kind"
  | isFullHouse hand = "Full house"
  | isThreeOfAKind hand = "Three of a kind"
  | isTwoPair hand = "Two pair"
  | isOnePair hand = "One pair"
  | isStraight hand = "Straight"
  | otherwise = "Bust"

-- Test functie
testCombinatie :: IO ()
testCombinatie = do
  let hands =
        [ [newDobbelsteen 1, newDobbelsteen 1, newDobbelsteen 1, newDobbelsteen 1, newDobbelsteen 1], -- Poker
          [newDobbelsteen 2, newDobbelsteen 2, newDobbelsteen 2, newDobbelsteen 2, newDobbelsteen 3], -- Four of a kind
          [newDobbelsteen 3, newDobbelsteen 3, newDobbelsteen 3, newDobbelsteen 4, newDobbelsteen 4], -- Full house
          [newDobbelsteen 4, newDobbelsteen 4, newDobbelsteen 4, newDobbelsteen 5, newDobbelsteen 6], -- Three of a kind
          [newDobbelsteen 5, newDobbelsteen 5, newDobbelsteen 6, newDobbelsteen 6, newDobbelsteen 1], -- Two pair
          [newDobbelsteen 6, newDobbelsteen 6, newDobbelsteen 2, newDobbelsteen 3, newDobbelsteen 4], -- One pair
          [newDobbelsteen 1, newDobbelsteen 2, newDobbelsteen 3, newDobbelsteen 4, newDobbelsteen 5], -- Straight
          [newDobbelsteen 1, newDobbelsteen 2, newDobbelsteen 3, newDobbelsteen 4, newDobbelsteen 6] -- Bust
        ]
  mapM_ (print . welkeCombinatie) hands

main :: IO ()
main = do
  hand <- newHand
  print hand
  putStrLn $ "Combinatie: " ++ welkeCombinatie hand

--  Hint: Probeer deze opdracht niet op te lossen door individuele functies te
-- schrijven voor poker, full house, etc. Schrijf, in plaats daarvan, een kleine
-- “bibliotheek” van functies die je kunt hergebruiken bij het oplossen van
-- meerdere varianten.