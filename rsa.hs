{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import System.Random
import Control.Monad

import System.IO
import Data.Char
import Data.List

import Graphics.UI.Gtk
import Control.Monad.Trans(liftIO)

import Data.Maybe
import Data.IORef
import Control.Concurrent
           

--a^x (mod m)
--gotten from the Crypto package online
expMod :: Integer -> Integer -> Integer -> Integer
expMod a x m |  x == 0    = 1
             |  x == 1    = a `mod` m
             |  even x    = let p = (expMod a (x `div` 2) m) `mod` m
                            in  (p^2) `mod` m
             |  otherwise = (a * expMod a (x-1) m) `mod` m

findD :: Integer -> IO Integer
findD phiN = do
              myStdGen <- newStdGen
              return $ head $ filter (\x -> gcd x phiN == 1 ) $ randomRs(2::Integer,(phiN-1)::Integer) myStdGen
            

--returns (n,e,d)
rsaInfo :: IO (Integer,Integer,Integer)
rsaInfo = do 
            p <- largePrime(2^2047,2^2048-1)
            q <- largePrime(2^2047,2^2048-1)            
            let n = p*q
                phiN = (p-1) * (q-1)

            d <- findD phiN      
            let (e,_) = extendedGCD d phiN            
 
            return (n,if e < 0 then phiN - e else e, d)


extendedGCD :: Integer -> Integer -> (Integer,Integer)
extendedGCD a b 
        | b == 0 = (1,0)
        | otherwise = (t,s-(q*t))
        where (q,r) = quotRem a b
              (s,t) = extendedGCD b r







largePrime (minBound,maxBound) = do
    myStdGen <- newStdGen

    return $  head $ filter (\n -> isProbablyPrime n 20) $ randomRs(minBound::Integer, maxBound::Integer) myStdGen


psudoprime a p = expMod a (p-1) p == 1

isProbablyPrime :: Integer -> Integer -> Bool
isProbablyPrime n k 
    | not (psudoprime 2 n) = False
    | not (psudoprime 3 n) = False
    | not (psudoprime 4 n) = False
    | not (psudoprime 5 n) = False
    | not (psudoprime 6 n) = False
    | not (psudoprime 7 n) = False
    | otherwise = all (\a -> failToGiveWitness a (expMod a d n) ) witnesses
   
   where witnesses = take (fromIntegral k) $ randomRs(2::Integer,(n-2)::Integer) myStdGen
         
         myStdGen = mkStdGen (fromIntegral n)

         failToGiveWitness a x
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
        
         findSandD n s
             | rem == 0 = findSandD newN (s+1) 
             | otherwise = (s,n)
             where (newN,rem) = divMod n 2

--some functions for seralizing a key
loadKey :: FilePath -> IO Key
loadKey f = do s <- readFile f
               return (read s)

saveKey :: Key -> FilePath -> IO ()
saveKey x f = writeFile f (show x)


--some functions for seralizing a key
loadCipher :: FilePath -> IO Cipher
loadCipher f = do s <- readFile f
                  return (read s)
 
saveCipher :: Cipher -> FilePath -> IO ()
saveCipher x f = writeFile f (show x)


--turns a string into an integer
integerFromText :: String -> Integer
integerFromText s  = sum [ (toInteger (ord c)) * 1000^x | (c,x) <- (zip s [0..]) ]



--turns an integer back into a string
textFromInteger :: Integer -> String
textFromInteger n 
        | q == 0 = [chr r]
        | otherwise = [chr r] ++ textFromInteger q 
      where r = fromIntegral r'
            (q,r') = quotRem n 1000

--getDataFromKey :: Key (Integer,Integer) -> (Integer,Integer)
getDataFromKey (Key d) = d

getDataFromCipher (Cipher cList) = cList

data Cipher = Cipher [Integer]
    deriving (Read, Show)

data Key = Key (Integer,Integer)
    deriving (Read, Show)


splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where (first,rest) = splitAt n list


encrypt :: Integer -> Integer -> Integer -> Integer
encrypt m n e = expMod m e n


decrypt :: Integer -> Integer -> Integer -> Integer
decrypt c n d = expMod c d n



main = do
    initGUI
    builder <- builderNew

   
    builderAddFromFile builder "rsaGUI.glade"
    mainWindow <- builderGetObject builder castToWindow "mainWindow"

    newKeypairButton <- builderGetObject builder castToButton "newKeypairButton"
    encryptButton <- builderGetObject builder castToButton "encryptButton"
    decryptButton <- builderGetObject builder castToButton "decryptButton"
   
    privateKeyFileChooser <- builderGetObject builder castToFileChooserButton "privateKeyFileChooser"
    publicKeyFileChooser <- builderGetObject builder castToFileChooserButton "publicKeyFileChooser"

    plaintextFileChooser <- builderGetObject builder castToFileChooserButton "plaintextFileChooser"
    ciphertextFileChooser <- builderGetObject builder castToFileChooserButton "ciphertextFileChooser"

    fileChooserSetCurrentFolder plaintextFileChooser "." 
    fileChooserSetCurrentFolder ciphertextFileChooser "." 
    
    fileChooserSetCurrentFolder publicKeyFileChooser "." 
    fileChooserSetCurrentFolder privateKeyFileChooser "." 

    log <- builderGetObject builder castToTextBuffer "log"

    

    clearButton <- builderGetObject builder castToButton "clearButton"
    copyButton <- builderGetObject builder castToButton "copyButton"
    saveButton <- builderGetObject builder castToButton "saveButton"


    windowSetDefaultIconFromFile "/usr/share/icons/gnome/scalable/apps/Haskell-Logo.svg"

    textBufferInsertAtCursor log $
        " • Welcome to λCrypt! This program generates RSA public and private keys, and uses them to decrypt and encrypt files.\n"

   -- windowSetGeometryHints mainWindow 
   --     (Just mainWindow) Nothing (Just (900,500))
   --     Nothing Nothing Nothing 

    --widgetSetSensitive ciphertextFileChooser False

    clearButton `on` buttonActivated $ do
        (start,end) <- textBufferGetBounds log
        textBufferDelete log start end

    copyButton `on` buttonActivated  $ do
        txt <- get log (textBufferText)
        cb <- clipboardGet selectionClipboard
        clipboardSetText cb txt    

    currentlyGenerating <- newIORef False

    mainWindow `on` keyPressEvent $ do
        k <- eventKeyVal 
        liftIO $ do
            if keyName k == "Escape"
                then mainQuit
                else return ()
        return False

    newKeypairButton `on` buttonActivated $ do
        
        curr <- readIORef currentlyGenerating        

        if curr
            then do
                    textBufferInsertAtCursor log $ 
                        " Hold 'yer horses! The keys are still generating, be patient.\n"
                    return ()
            else do      
                    atomicWriteIORef currentlyGenerating True
                    widgetSetSensitive newKeypairButton False                    
                    forkIO $ do           
                        postGUISync $ do
                            textBufferInsertAtCursor log $    
                                " • Generating the RSA keys. This may take awhile.\n"                   
                        
                            
      
                        (n,e,d) <- rsaInfo 
                        
                        let privateKey = Key (n,d)
                            publicKey  = Key (n,e)
                            

                        saveKey privateKey ("default.pri")
                        saveKey publicKey ("default.pub")


                        postGUISync $ do
                            textBufferInsertAtCursor log $
                                " • RSA keys written to default.pri and default.pub\n"   
                        atomicWriteIORef currentlyGenerating False
                        widgetSetSensitive newKeypairButton True 
                    return ()

    encryptButton `on` buttonActivated $ do 
         plaintextFilePath <- fileChooserGetFilename plaintextFileChooser
         publicKeyFilePath <- fileChooserGetFilename publicKeyFileChooser


         forkFinally  
            (do 
                   
                postGUISync $ 
                    textBufferInsertAtCursor log $
                        " • Encrypting data...\n"  

                
                rawMessage <- readFile $ fromJust plaintextFilePath
                
            
                k <- loadKey $ fromJust publicKeyFilePath
            
            
                let blocks = map integerFromText $ splitEvery 50 rawMessage
                    (n,e) = getDataFromKey k
                    cList = map (\m -> encrypt m n e) blocks
                    
                saveCipher (Cipher cList) (fromJust plaintextFilePath ++ ".cipher")
         
                postGUISync $ 
                    textBufferInsertAtCursor log $
                         " • Encrypted data written to " ++ fromJust plaintextFilePath ++ ".cipher\n"
             )
            ( \e -> do{putStrLn ("Thread exited with exception:  " ++ show e);widgetSetSensitive encryptButton True;widgetSetSensitive plaintextFileChooser True })
         widgetSetSensitive plaintextFileChooser False
         widgetSetSensitive encryptButton False

  
    decryptButton `on` buttonActivated $ do
         ciphertextFilePath <- fileChooserGetFilename ciphertextFileChooser
         privateKeyFilePath <- fileChooserGetFilename privateKeyFileChooser

        
         widgetSetSensitive ciphertextFileChooser False
         widgetSetSensitive decryptButton False
                  
         
         -- TODO: Fix to fork finally, so widgets set sensitive, and remove the fromJust possibility, and parse error posibility. And
         -- figure out why in the world I have to declare the widgets insensitive after the fork. My guess is file isn't being 
         -- chosen from the fileSelector strictly?
        
         _ <- forkFinally
               ( do
            
                    postGUISync $ 
                         textBufferInsertAtCursor log $
                             " • Decrypting data...\n"
                    
                                      -- needs to be in here or thread won't work. I'm guessing that the inital call to SetSensitive is queued and doesn't 
                                      -- take effect until instantly before the setSensitive here. Doesn't make sense though, something's off.
                                      -- HA! I get it! the computation is all taking place in a thunk which is passed to the main gui through
                                      -- Async!! Ha.    


                    
                    ciph <- loadCipher $ fromJust ciphertextFilePath
                    k <- loadKey $ fromJust privateKeyFilePath

                    let (n,d) = getDataFromKey k
                        cList = getDataFromCipher ciph
                        m = concatMap (\c -> textFromInteger (decrypt c n d)) cList
                        
                    
                    mStrict <- return $! m --force strictness, so postGUIAsync is only posted when the message is decrypted. I could also make it a sync?                 
                                           -- I changed it to a sync, just to make it a bit cleaner and more ordered.
    

                                       
                    postGUISync $ 
                       textBufferInsertAtCursor log $
                           " • The decrypted data is as follows:\n" ++ mStrict  ++ "\n"  --lag here on large texts being posted to textBuffer. TODO
                    
                    widgetSetSensitive decryptButton True
                    widgetSetSensitive ciphertextFileChooser True
                )
               (\e -> do{putStrLn ("Thread exited with exception " ++ show e);widgetSetSensitive decryptButton True;widgetSetSensitive ciphertextFileChooser True})
         return()   
         

    mainWindow `on` deleteEvent $ do
         liftIO
             mainQuit
         return False


    widgetShowAll mainWindow

    mainGUI

--decryptFile 
--decryptFile cipherText 
