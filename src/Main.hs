module Main (main) where

--import my own modules
import Primality
import RSA


import Graphics.UI.Gtk --this is from the gtk3 package
import Control.Monad.Trans(liftIO)
import Data.Maybe
import Control.Concurrent


import Paths_LambdaCrypt


main :: IO ()
main = do
    _ <- initGUI
    builder <- builderNew

    gladePath <- getDataFileName "resources/rsaGUI.glade"
    builderAddFromFile builder gladePath
    mainWindow <- builderGetObject builder castToWindow "mainWindow"

    newKeypairButton <- builderGetObject builder castToButton "newKeypairButton"
    encryptButton <- builderGetObject builder castToButton "encryptButton"
    decryptButton <- builderGetObject builder castToButton "decryptButton"
   
    privateKeyFileChooser <- builderGetObject builder castToFileChooserButton "privateKeyFileChooser"
    publicKeyFileChooser <- builderGetObject builder castToFileChooserButton "publicKeyFileChooser"

    plaintextFileChooser <- builderGetObject builder castToFileChooserButton "plaintextFileChooser"
    ciphertextFileChooser <- builderGetObject builder castToFileChooserButton "ciphertextFileChooser"

    -- load the key generation popup window and sub-widgets
    keyGenDialog <- builderGetObject builder castToDialog "keyGenDialog"
    keyGenFolderChooser <- builderGetObject builder castToFileChooserButton "keyGenFolderChooser"
    keyGenNameEntry <- builderGetObject builder castToEntry "keyGenNameEntry"
    keyGenUpper <- builderGetObject builder castToTable "keyGenUpper"
    -------------------------------------------------------
    
    -- prepare the dialog
    
    _ <- dialogAddButton keyGenDialog "gtk-cancel" ResponseCancel
    _ <- dialogAddButton keyGenDialog "gtk-apply" ResponseApply
    
    contents <- dialogGetContentArea keyGenDialog  
    
    let contentsVbox= castToBox contents
       in boxPackStart contentsVbox keyGenUpper PackGrow 0 
    -----------------------------------------------------------
    
    _ <- fileChooserSetCurrentFolder plaintextFileChooser "." 
    _ <- fileChooserSetCurrentFolder ciphertextFileChooser "." 
    
    _ <- fileChooserSetCurrentFolder publicKeyFileChooser "." 
    _ <- fileChooserSetCurrentFolder privateKeyFileChooser "." 

    _ <- fileChooserSetCurrentFolder keyGenFolderChooser "."


    log <- builderGetObject builder castToTextBuffer "log"


    clearButton <- builderGetObject builder castToButton "clearButton"
    copyButton <- builderGetObject builder castToButton "copyButton"
    saveButton <- builderGetObject builder castToButton "saveButton"


    --windowSetDefaultIconFromFile "/usr/share/icons/gnome/scalable/apps/Haskell-Logo.svg"
    iconPath <- getDataFileName "resources/Haskell-Logo.svg"
    _ <- windowSetDefaultIconFromFile iconPath

    textBufferInsertAtCursor log $
        " • Welcome to λCrypt! This program generates RSA public and private keys, and uses them to decrypt and encrypt files.\n"


    _ <- clearButton `on` buttonActivated $ do
        (start,end) <- textBufferGetBounds log
        textBufferDelete log start end

    _ <- copyButton `on` buttonActivated  $ do
        txt <- get log (textBufferText)
        cb <- clipboardGet selectionClipboard
        clipboardSetText cb txt    

   
    _ <- mainWindow `on` keyPressEvent $ do
           k <- eventKeyVal 
           liftIO $ do
              if keyName k == "Escape"
                  then mainQuit
                  else return ()
           return False

    _ <- newKeypairButton `on` buttonActivated $ do
        
        widgetSetSensitive newKeypairButton False                    
        resp <- dialogRun keyGenDialog
        
            
        if resp == ResponseApply
          then do
            name <- entryGetText keyGenNameEntry
            path <- fileChooserGetFilename keyGenFolderChooser 
            --TODO: Enforce folder selection/name entry
            --currently can crash the thread with a fromJust (Nothing)
            
            forkIO $ do           
                postGUISync $ do
                    textBufferInsertAtCursor log $    
                        " • Generating the RSA keys. This may take awhile.\n"                   
                                
                let fullPath = (fromJust path) ++ "/" ++ name                   
                
                generateAndSaveKeys fullPath
     
        
                postGUISync $ do
                    textBufferInsertAtCursor log $
                        " • RSA keys written to " ++ fullPath ++ ".pri and " ++ fullPath ++ ".pub\n"   
                widgetSetSensitive newKeypairButton True
            widgetHide keyGenDialog
            return ()
         else do
            widgetHide keyGenDialog
            widgetSetSensitive newKeypairButton True               
            return ()
        

    _ <- encryptButton `on` buttonActivated $ do 
         plaintextFilePath <- fileChooserGetFilename plaintextFileChooser
         publicKeyFilePath <- fileChooserGetFilename publicKeyFileChooser

   --TODO: polish up these callbacks, separate it into functions
         _ <- forkFinally  
            (do 
                
                postGUISync $ 
                    textBufferInsertAtCursor log $
                        " • Encrypting data...\n"  
        
                --throws an exception (caught by forkFinally) if either paths are nothing
                --TODO: make it so that Nothings are taken into consideration
                encryptToFile (fromJust plaintextFilePath) (fromJust publicKeyFilePath)     
                
                postGUISync $ 
                    textBufferInsertAtCursor log $
                         " • Encrypted data written to " ++ fromJust plaintextFilePath ++ ".cipher\n"
             )
            ( \e -> do{putStrLn ("Thread exited with exception:  " ++ show e);widgetSetSensitive encryptButton True;widgetSetSensitive plaintextFileChooser True})
         widgetSetSensitive plaintextFileChooser False
         widgetSetSensitive encryptButton False

  
    _ <- decryptButton `on` buttonActivated $ do
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
                                
                            --TODO: Assumes the Maybe is Just. If not, it's caught by ForkFinally,
                            --but still should be fixed to give an informative message
                            m <- decryptFile (fromJust ciphertextFilePath) (fromJust privateKeyFilePath)
       
                                               
                            postGUISync $ 
                               textBufferInsertAtCursor log $
                                   " • The decrypted data is as follows:\n" ++ m  ++ "\n"  --lag here on large texts being posted to textBuffer. TODO
                            
                            widgetSetSensitive decryptButton True
                            widgetSetSensitive ciphertextFileChooser True
                        )
                       (\e -> do{putStrLn ("Thread exited with exception " ++ show e);widgetSetSensitive decryptButton True;widgetSetSensitive ciphertextFileChooser True})
                 return()   
                 
                              
    _ <- mainWindow `on` deleteEvent $ do
           liftIO
               mainQuit
           return False


    widgetShowAll mainWindow 

    mainGUI