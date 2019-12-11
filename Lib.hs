module Lib
    ( someFunc
    ) where

import GRM.Abs 
import GRM.Par (pCmd, myLexer)
import GRM.ErrM
import System.Process
import Control.Exception

someFunc :: IO ()
someFunc = do
    cmdstr <- getLine
    case pCmd (myLexer cmdstr) of
            Ok t ->  interpret t
            Bad err -> putStrLn err
    someFunc


interpret :: Cmd -> IO ()
interpret (Cmd []) = return ()
interpret (Cmd prgs) = mapM_ progrun prgs

try' :: IO a ->  IO (Either IOException a)
try' =  try 

progrun :: Prg -> IO ()
progrun (NPrg (Ident p) args) = do 
    res <- try' $ (createProcess (proc p []))
    case res of 
        Left ex -> putStrLn $ show ex
        Right _ -> return ()
    return ()

