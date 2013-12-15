module NumberTheory
(expMod
,extendedGCD
) where

--this module contains various functions necessary for 
--a few other modules in the program. They could of course
--be imported from another package, but this decreases
--dependencies. These were found online.



--a^x (mod m)
expMod :: Integer -> Integer -> Integer -> Integer
expMod a x m |  x == 0    = 1
             |  x == 1    = a `mod` m
             |  even x    = let p = (expMod a (x `div` 2) m) `mod` m
                            in  (p^2) `mod` m
             |  otherwise = (a * expMod a (x-1) m) `mod` m



extendedGCD :: Integer -> Integer -> (Integer,Integer)
extendedGCD a b 
        | b == 0 = (1,0)
        | otherwise = (t,s-(q*t))
        where (q,r) = quotRem a b
              (s,t) = extendedGCD b r
