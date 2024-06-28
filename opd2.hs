import Data.List (find)
import Data.Maybe (fromJust)

-- Inleiding
-- In deze practicumopgave gaan we ons bezighouden met encryptie. We richten
-- ons daarbij op RSA encryptie, te weten:
--  conversie van een tekst naar de ascii waarden van die tekst
--  priv´e en publieke sleutel generatie
--  rsa encryptie van een bericht
--  rsa decryptie van een bericht
-- Aan het einde van deze opdracht ben je in staat sleutels te genereren en een
-- gegeven bericht te versleutelen en ontsleutelen. Je schrijft dus een volledig
-- werkende implementatie van dit veelgebruikte algoritme.
-- Voor deze opdracht heb je een aantal hulpfuncties nodig. Deze functies
-- schrijf je als eerst, zodat je ze verderop in de opdracht kunt gebruiken. Schrijf
-- de volgende functies:

-- Opdracht 1a
-- euclid::Integer->Integer->Integer
-- euclid x y
-- Deze functie berekent de grootste gemene deler van twee gegeven natuurlijke
-- getallen x en y. Let op de type signature. Deze werkt met Integer, zodat je
-- met grote getallen kunt werken als dat nodig is.

-- UwU
-- >_<
-- xD
euclid::Integer->Integer->Integer -- for up to half of int1, check if both divisible
euclid a b = finddivisible a b a


finddivisible::Integer->Integer->Integer->Integer
finddivisible a b c
  | c < 0 = -1
  | mod a c == 0 && mod b c == 0 = c
  | otherwise = finddivisible a b (c - 1)


-- GCD (a, b) = (a × b)/ LCM(a, b)
mygcd a b = (a * b) `div` mylcm a b

-- LCM (a,b) = (a × b) ÷ HCF(a,b)
mylcm a b = (a * b) `div` commonfactors a b

-- We will list the factors of 30 and 42. The factors of 30 are 1, 2, 3, 5, 6, 10, 15, and 30 and the factors of 42 are 1, 2, 3, 6, 7, 14, 21, and 42.
--  Clearly, 1, 2, 3, and 6 are the common factors of 30 and 42. 
-- But 6 is the greatest of all the common factors. Hence, the HCF of 30 and 42 is 6

commonfactors::Integer->Integer->Integer
commonfactors a b = maximum [x | x <- [1..a], y <- [1..b], x == y, a `mod` x == 0, b `mod` y == 0]

-- Opdracht 1b
-- Gegeven de volgende congruentie:
-- e · d = 1(mod m)
-- Een dergelijke berekening is nodig voor het genereren van de publieke en priv´e
-- sleutel. Een algoritme, waarmee de gegeven congruentie is op te lossen is het
-- volgende:

-- Extended Euclidean Algorithm
egcd :: Integer -> Integer -> (Integer, Integer, Integer)
egcd 0 b = (b, 0, 1)
egcd a b =
  let (g, s, t) = egcd (b `mod` a) a
  in (g, t - (b `div` a) * s, s)

-- Correcting negative values to be positive
egcdPos :: Integer -> Integer -> (Integer, Integer, Integer)
egcdPos a b =
  let (g, x, y) = egcd a b
      x' = if x < 0 then x + b else x
      y' = if y < 0 then y + a else y
  in (g, x', y')

-- Helaas levert dit algoritme soms een negatieve uitkomst. Gebruikt het algoritme
-- in een eigen
-- ghci> egcdPos 32 162sleutel: strikt geheim en in jouw persoonlijke bezit.
--  de publieke sleutel: deze mag iedereen hebben.
-- Voor het genereren van sleutels zijn twee priemgetallen, p en q nodig. Kies
-- twee priemgetallen. Hou deze getallen klein, d.w.z. tussen de 100 en 500. In
-- echte toepassingen van rsa encryptie zijn de getallen veel groter en worden veel
-- eﬃci¨entere algoritmen gebruikt. Deze algoritmen maken gebruik van geavan-
-- ceerdere getaltheorie. In deze opdracht houden we het bij de basale werking
-- van rsa encryptie.
-- De volgende berekeningen moeten worden uitgevoerd:
--  De modulus: m = p · q
--  Eulers totient functie: m′ = φ(m) = (p − 1) · (q − 1)
modulus :: Integer -> Integer -> Integer
modulus p q = p * q

eulers:: Integer -> Integer -> Integer
eulers p q = (p - 1) * (q - 1)

-- Function to find the modular inverse
modInverse :: Integer -> Integer -> Integer
modInverse e phi =
  let (_, d, _) = egcd e phi
  in d `mod` phi

  -- Generate RSA Keys
generateRSAKeys :: Integer -> Integer -> ((Integer, Integer), (Integer, Integer))
generateRSAKeys p q = 
  let m = modulus p q
      phi = eulers p q
      e = fromJust $ find (\x -> coprime x phi) [2..phi-1]
      d = modInverse e phi
  in ((e, m), (d, m))  -- (Public Key, Private Key)


-- RSA Key Generation
-- generateRSAKeys :: Integer -> Integer -> ((Integer, Integer), (Integer, Integer))
-- generateRSAKeys p q = 
--   let m = p * q
--       phi = (p - 1) * (q - 1)
--       e = fromJust $ find (\x -> gcd x phi == 1) [2..phi-1]
--       d = fromJust $ modInv e phi
--   in ((e, m), (d, m))  -- (Public Key, Private Key)

-- Vervolgens kiezen we een getal e dat relatief priem is met m′. Het getal e
-- voldoet dus aan de volgende twee voorwaarden:
--  e < m′
--  ggd(e, m′) = 1
-- Zodra we een geschikt getal e gekozen hebben, moeten we een bijbehorende d
-- berekenen. Voor d geldt:
--  e · d = 1(mod m′)
-- We hebben nu de priv´e sleutel, de publieke sleutel en de modulus!
--  De priv´e sleutel is e
--  De publieke sleutel is d
--  De modulus is m
-- 3
-- Opdracht 3a: rsa encryptie
-- Schrijf een functie die een tuple van sleutel en modulus, alsmede een getal als
-- parameters heeft en dit getal versleutelt:
-- rsaencrypt::(Integer,Integer)->Integer->Integer
-- rsaencrypt (e,m) x
-- Opdracht 3b: rsa decryptie
-- Schrijf een functie die een tuple van sleutel en modulus, alsmede een getal als
-- parameters heeft en dit getal ontsleutelt:
-- rsadecrypt::(Integer,Integer)->Integer->Integer
-- rsadecrypt (d,m) x
-- Opdracht 4
-- Versleutel en ontsleutel ´e´en letter m.b.v. de functies uit opdracht 3. Voor de
-- conversie van een letter naar een ascii waarde en vice versa zijn twee handige
-- functies beschikbaar, te vinden in de bibliotheek Data.Char:
--  ord
--  chr
-- Oefen met beide functies.
-- Opdracht 5
-- Alice en Bob willen veilig met elkaar communiceren. Echter: een bericht dat
-- door Alice met haar priv´e sleutel werd versleuteld, kan door iedereen met de
-- publieke sleutel worden ontsleuteld. Hoe kun je rsa gebruiken om Alice en Bob
-- toch veilig met elkaar te laten communiceren?
-- Opdracht 6 (facultatief )
-- Simuleer een man in the middle attack