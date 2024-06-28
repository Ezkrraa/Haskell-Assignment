import Control.Arrow (Arrow (first))
import Data.Bits (Bits (shiftL, shiftR))

-- Opdracht 1a
-- Schrijf de functie

-- Deze functie berekent de faculteit en maakt daarbij gebruik van pattern mat-ching.
faca :: Int -> Int
faca 0 = 1
faca x = product [1 .. x]

-- Opdracht 1b
-- Deze functie berekent de faculteit en maakt daarbij gebruik van guards.
-- Schrijf de functie

facb :: Int -> Int
facb x
  | x == 0 = 1
  | otherwise = x * facb (x - 1)

-- Opdracht 2a
-- Schrijf de functie
-- Deze functie berekent de nulpunten van een tweedegraads functie.

nulpuntena :: Double -> Double -> Double -> [Double]
nulpuntena a b c =
  let discriminant = b * b - 4 * a * c
      discSqrRoot = sqrt discriminant
      posRoot = (-b + discSqrRoot) / (2 * a)
      negRoot = (-b - discSqrRoot) / (2 * a)
   in if discriminant > 0
        then [posRoot, negRoot]
        else ([negRoot | discriminant == 0])

-- Opdracht 2b
-- Schrijf de functie
-- Deze functie berekent de nulpunten van een tweedegraads functie en maakt
-- daarbij gebruik van het where keyword en guards.

nulpuntenb :: Double -> Double -> Double -> [Double]
nulpuntenb a b c
  | discriminant > 0 = [posRoot, negRoot]
  | discriminant == 0 = [posRoot]
  | discriminant < 0 = []
  where
    discriminant = b * b - 4 * a * c
    discSqrRoot = sqrt discriminant
    posRoot = (-b + discSqrRoot) / (2 * a)
    negRoot = (-b - discSqrRoot) / (2 * a)

-- Opdracht 2c
-- We gooien met drie dobbelstenen. Schrijf een functie die een lijst teruggeeft
-- met alle mogelijke worpen. Voor het representeren van één worp gebruiken we
-- tuples, bijv. (1, 2, 3). Schrijf de functie zodanig dat alleen de worpen, waarvan
-- de som van de ogen een veelvoud is van vijf in de lijst worden opgenomen. Hoe
-- groot is het aantal worpen?

worpenvan5 = ([(a, b, c) | a <- [1 .. 6], b <- [1 .. 6], c <- [1 .. 6], mod (a + b + c) 5 == 0], length (fst worpenvan5))

-- Opdracht 2d
-- Herschrijf de functie uit opdracht 2c zodanig dat de lijst van tuples een veelvoud
-- is van n.

worpenvan n = ([(a, b, c) | a <- [1 .. 6], b <- [1 .. 6], c <- [1 .. 6], mod (a + b + c) n == 0], length (fst (worpenvan n)))

-- Opdracht 3
-- Los de volgende puzzel op met Haskell:
-- We hebben 3 gehele getallen:
--  Het eerste getal is 2 keer het verschil van het tweede en derde getal.
--  Het tweede getal is het product van het eerste en derde getal.
--  Het derde getal is de helft van de som van het eerste en tweede getal.
-- Wat zijn de waarden van het eerste, tweede en derde getal?
-- puzzle :: [(Integer, Integer, Integer)]
-- puzzle = [(x, y, z) | x <- [-100 .. 100], z <- [-100 .. 100], let y = x * z, x == (y - z) * 2, z == (x + y) `div` 2]

-- Opdracht 4a
-- Twee positieve gehele getallen kunnen met elkaar worden vermenigvuldigd zon-
-- der daarbij gebruik te maken van de (*) functie.
--  Deze functie is gebaseerd op de gedachte dat vermenigvuldigen hetzelfde
-- is als herhaald optellen.
--  Test je functie door steeds grotere getallen met elkaar te vermenigvuldigen.
-- Ga door tot de Haskell interpreter het genoeg vindt en je getrakteerd wordt
-- op een stack overflow. Zet de getallen die een stack overflow veroorzaken
-- in het commentaar van je source.


mult :: Integer -> Integer -> Integer
-- mult a b = sum [a | x <- [1..b], x <= b]

mult a b = multHelper a b 0

multHelper a 0 result = result
multHelper a b result = multHelper a (b - 1) (result + a)

-- Opdracht 4b
-- Schrijf een willekeurig even getal x in bits uit. Schrijf ook uit: 2x, 4x en ook
-- 1
-- 1
-- 2 x, 4 x. Doe hetzelfde met een oneven getal Wat valt je op?
--  Schrijf de functie fastmult::Integer->Integer->Integer
--  Deze functie vermenigvuldigt twee positieve getallen op een veel efficiëntere
-- manier.
--  Test je functie met dezelfde getallen als in opdracht 4a.

fastmult a b = fastmulthelper a b 0

fastmulthelper a 0 result = result
fastmulthelper a b result
  | odd b = fastmulthelper (a * 2) (b `div` 2) (result + a)
  | otherwise = fastmulthelper (a * 2) (b `div` 2) result

-- Opdracht 5a
--  Schrijf de functie pow::Integer->Integer->Integer. De functie re-
-- kent uit: xp , x > 0, p > 0.
--  Deze functie is gebaseerd op de gedachte dat machtsverheffen hetzelfde
-- is als herhaald vermenigvuldigen.
--  Test je functie met steeds grotere getallen. Ga door tot de Haskell in-
-- terpreter het genoeg vindt en je getrakteerd wordt op een stack overflow.
-- Zet de getallen die een stack overflow veroorzaken in het commentaar van
-- je source.
pow :: Integer -> Integer -> Integer
pow x p = powHelper x p x

powHelper x 1 result = result
powHelper x p result = powHelper x (p - 1) (result * x)

-- Opdracht 5b
--  Schrijf de functie fastpow::Integer->Integer->Integer. De functie
-- rekent uit: xp .
--  Deze functie maakt handig gebruik van bitshifting.
--  Test je functie met dezelfde getallen als in opdracht 5a.

fastPow x p = fastPowHelper x p 1

fastPowHelper x 0 result = result
fastPowHelper x p result
  | odd p = fastPowHelper (x * x) (p `shiftR` 1) (result * x)
  | otherwise = fastPowHelper (x * x) (p `shiftR` 1) result