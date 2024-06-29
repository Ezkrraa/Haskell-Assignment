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

-- Generate the next state and a random number from the current state
lcg :: Seed -> (Integer, Seed)
lcg seed =
  let newSeed = (a * seed + c) `mod` m
   in (newSeed, newSeed)

-- Function to initialize the generator with a given seed
initLCG :: Seed -> Seed
initLCG seed = seed

-- Example function to get a list of n random numbers
randomNumbers :: Seed -> Int -> [Integer]
randomNumbers _ 0 = []
randomNumbers seed n =
  let (num, newSeed) = lcg seed
   in num : randomNumbers newSeed (n - 1)

getCurrentTimeSeed :: IO Seed
getCurrentTimeSeed = do
  time <- getPOSIXTime
  return (round (time * 1000))

main :: IO ()
main = do
  seed <- getCurrentTimeSeed
  let num = randomNumbers seed 10
  print num
  seed <- getCurrentTimeSeed
  let numbers = randomNumbers seed 10
  print numbers
  let numbers = randomNumbers seed 10
  print numbers
  let numbers = randomNumbers seed 10
  print numbers
