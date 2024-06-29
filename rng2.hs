import Data.Time.Clock.POSIX (getPOSIXTime)

-- Linear Congruential Generator parameters
m :: Integer
m = 2 ^ 31

a :: Integer
a = 1103515245

c :: Integer
c = 12345

-- Type synonym for the state of the generator
type Seed = Integer

-- Function to generate a random number using a given seed and a maximum value
generateRandomNumber :: Seed -> Integer -> Integer
generateRandomNumber seed maxVal =
  let (randomNum, _) = lcg seed
   in (randomNum `mod` maxVal) + 1

-- Generate the next state and a random number from the current state
lcg :: Seed -> (Integer, Seed)
lcg seed =
  let newSeed = (a * seed + c) `mod` m
   in (newSeed, newSeed)

-- Function to get the current time in milliseconds and use it as a seed
getCurrentTimeSeed :: IO Seed
getCurrentTimeSeed = do
  time <- getPOSIXTime
  return (round (time * 1000000)) -- Multiply by a larger factor for uniqueness

main :: IO ()
main = do
  -- Get an initial seed based on current time
  initialSeed <- getCurrentTimeSeed
  -- Generate 100 random numbers using successive seeds
  let randomNumbers = take 100 $ map (`generateRandomNumber` 6) $ iterate (snd . lcg) initialSeed
  print randomNumbers

-- Claude > ChatJippity, 3.5 sonnet > gpt3.5