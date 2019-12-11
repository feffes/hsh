module Lib
    ( someFunc
    ) where

import GRM.Abs 
import GRM.Par (pCmd, myLexer)
import GRM.ErrM
import System.Process

someFunc :: IO ()
someFunc = do
    cmdstr <- getLine
    let tree = case pCmd (myLexer cmdstr) of
            Ok t -> t
            Bad err -> error err
    interpret tree
    return ()


interpret :: Cmd -> IO ()
interpret (Cmd []) = return ()
interpret (Cmd prgs) = mapM_ progrun prgs


progrun :: Prg -> IO ()
progrun (NPrg (Ident p) args) = do 
    proc <- createProcess (proc p [])
    return ()

