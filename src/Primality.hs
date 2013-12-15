module Primality
( largePrime
)

where 

import NumberTheory

import System.Random


--generates a large prime in the range specified.
--this may take awhile, as
--TODO: Generate the random large numbers more efficiently
largePrime :: (Integer,Integer) -> IO Integer
largePrime (lowerBound,upperBound) = do
    myStdGen <- newStdGen

    return $  head $ filter (\n -> isProbablyPrime n 20) $ randomRs(lowerBound, upperBound) myStdGen

pseudoprime :: Integer -> Integer -> Bool
pseudoprime a p = expMod a (p-1) p == 1


--this determines if an integer is probably prime.
--the chance that this gives a false positive is 4^(-k)
--all negatives are true negatives.
isProbablyPrime :: Integer -> Integer -> Bool
isProbablyPrime n k 
    | any (\b -> not (pseudoprime b n)) [2..7] = False --makes sure that n is at least a base-b pseudoprime (where b <- [2..7])
    -- | not (pseudoprime 2 n) = False
    -- | not (pseudoprime 3 n) = False
    -- | not (pseudoprime 4 n) = False
    -- | not (pseudoprime 5 n) = False
    -- | not (pseudoprime 6 n) = False
    -- | not (pseudoprime 7 n) = False
    | otherwise = all (\a -> failToGiveWitness (expMod a d n) ) witnesses
   
   where witnesses = take (fromIntegral k) $ randomRs(2::Integer,(n-2)::Integer) myStdGen
         
         myStdGen = mkStdGen (fromIntegral n) -- TODO: the std gen is seeded with the number that's being checked for primaility. This probably lacks rigor.

         failToGiveWitness x
             | x == 1 || x == (n-1)     = True       
             | failToGiveWitnessInner(s-1) x  = True 
             | otherwise                = False
 
         failToGiveWitnessInner counter x
             | counter <= 0 = False
             | newX == 1 = False     
             | newX == (n-1) = True
             | otherwise =  failToGiveWitnessInner (counter-1) newX
             where newX = expMod x 2 n

         (s,d) = findSandD (n-1) 0

findSandD :: Integer -> Integer -> (Integer, Integer)
findSandD n s
     | remainder == 0 = findSandD newN (s+1) 
     | otherwise = (s,n)
     where (newN,remainder) = divMod n 2

