module RSA
( encryptToFile
, decryptFile  
, generateAndSaveKeys    
)
 where

import NumberTheory
import Primality

import System.Random
import Data.Char

--TODO: make it so this accepts bit-length and how much certainty of the prime-generation
-- used in the process the user would like
generateAndSaveKeys :: String -> IO()
generateAndSaveKeys name = do

                (n,e,d) <- rsaInfo 
                
                let privateKey = Key (n,d)
                    publicKey  = Key (n,e)
        
                saveKey privateKey (name ++ ".pri")
                saveKey publicKey (name ++ ".pub")


--takes a path to a file to encrypt, a path file
--to a public key, and encrypts the data to
--the file originalfile.cipher
encryptToFile :: String -> String -> IO ()
encryptToFile plaintextFilePath publicKeyFilePath = do
       
        message <- readFile plaintextFilePath --lazy read, perhaps use strict instead?
        k <- loadKey publicKeyFilePath
    
        let ciphertext = encrypt message k
         
        saveCipher ciphertext (plaintextFilePath ++ ".cipher") 
        
   
decryptFile :: String -> String -> IO String
decryptFile ciphertextFilePath privateKeyFilePath = do
    ciphertext <- loadCipher ciphertextFilePath
    privatekey <- loadKey privateKeyFilePath
          
    return $! decrypt ciphertext privatekey 
 
 
 
encrypt :: String -> Key -> Cipher
encrypt plaintext privatekey  
    = Cipher $ map (\m -> encryptRSA m e n) blocks
    where blocks = map integerFromText $ splitEvery 50 plaintext
          (n,e) = getDataFromKey privatekey
                    
    

decrypt :: Cipher -> Key -> String
decrypt ciphertext publickey 
    = concatMap (\c -> textFromInteger (decryptRSA c d n)) blocks
    where (n,d) = getDataFromKey publickey
          blocks = getDataFromCipher ciphertext

      
--encrypts a large integer, m, by the public exponent
--and the public modulus. 
encryptRSA :: Integer -> Integer -> Integer -> Integer
encryptRSA m e n = expMod m e n

--decryptes a large integer, b, by the private exponent
--and the public modulus
decryptRSA :: Integer -> Integer -> Integer -> Integer
decryptRSA c d n = expMod c d n


findD :: Integer -> IO Integer
findD phiN = do
              myStdGen <- newStdGen
              return $ head $ filter (\x -> gcd x phiN == 1 ) $ randomRs(2::Integer,(phiN-1)::Integer) myStdGen
            

--returns (n,e,d) RSA values,
--where e the public exponent, d is the private exponent, 

rsaInfo :: IO (Integer,Integer,Integer)
rsaInfo = do 
            p <- largePrime(2^2047,2^2048-1)
            q <- largePrime(2^2047,2^2048-1)            
            let n = p*q
                phiN = (p-1) * (q-1)

            d <- findD phiN      
            let (e,_) = extendedGCD d phiN            
 
            return (n,if e < 0 then phiN - e else e, d) --if E is negative, subtract it from phi(N) to get a congruent positive value



         
--TODO: These below functions (up to the "data Key" declaration) are inefficent, both in time and space. 
--And limits the encryption/decryption to ASCII text.
--I should use a binary or bytestream model instead.

--some functions for seralizing a key
loadKey :: FilePath -> IO Key
loadKey f = do s <- readFile f
               return (read s)

saveKey :: Key -> FilePath -> IO ()
saveKey x f = writeFile f (show x)


--some functions for seralizing the ciphertext
loadCipher :: FilePath -> IO Cipher
loadCipher f = do s <- readFile f
                  return (read s)
 
saveCipher :: Cipher -> FilePath -> IO ()
saveCipher x f = writeFile f (show x)


--turns a string into an integer
integerFromText :: String -> Integer
integerFromText s  = sum [ (toInteger (ord c)) * 1000^x | (c,x) <- (zip s [(0::Integer)..]) ]


--turns an integer back into a string
textFromInteger :: Integer -> String
textFromInteger n 
        | q == 0 = [chr r]
        | otherwise = [chr r] ++ textFromInteger q 
      where r = fromIntegral r'
            (q,r') = quotRem n 1000

--getDataFromKey :: Key (Integer,Integer) -> (Integer,Integer)
getDataFromKey :: Key -> (Integer,Integer)
getDataFromKey (Key k) = k

getDataFromCipher :: Cipher -> [Integer]
getDataFromCipher (Cipher cList) = cList

data Cipher = Cipher [Integer]
    deriving (Read, Show)

data Key = Key (Integer,Integer)
    deriving (Read, Show)


--utility function for splitting message into blocks
splitEvery ::Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where (first,rest) = splitAt n list

