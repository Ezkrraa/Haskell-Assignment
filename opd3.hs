import Data.List

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
integreer f a b p = sum [f (a + i * p) * p | i <- [0 .. (b - a) / p]] -- 💀

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

testdubbelen :: IO ()
testdubbelen = do
  print $ dubbelen "aabbbcdeeeef"

-- Deze functie heeft een lijst als invoer en levert als uitvoer een lijst met uitsluitend
-- die elementen die meer dan een keer voorkomen in de lijst. Voorbeeld:
-- ghci>dubbelen "aabbbcdeeeef"
-- ghci>"abe"
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
-- 3
--  Hint: Probeer deze opdracht niet op te lossen door individuele functies te
-- schrijven voor poker, full house, etc. Schrijf, in plaats daarvan, een kleine
-- “bibliotheek” van functies die je kunt hergebruiken bij het oplossen van
-- meerdere varianten.
-- 4