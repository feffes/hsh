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
    let arguments = map getArg args
    res <- try' $ createProcess (proc p arguments)
    case res of 
        Left ex -> print ex
        Right _ -> return ()
    return ()

getArg :: Arg -> String
getArg (NArg arg) = arg