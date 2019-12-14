module Lib
    ( someFunc
    ) where

import GRM.Abs 
import GRM.Par (pCmd, myLexer)
import GRM.ErrM
import System.Process
import Control.Exception
import System.Directory
import System.IO
import Control.Monad
import Control.Monad.IO.Class

someFunc :: IO ()
someFunc = do
    hSetBuffering stdout NoBuffering
    str <- getCurrentDirectory
    putStr (str ++ " > ")
    cmdstr <- getLine
    case pCmd (myLexer cmdstr) of
            Ok t ->  interpret t
            Bad err -> putStrLn err
    someFunc


interpret :: Cmd -> IO ()
interpret (Cmd []) = return ()
interpret (Cmd [x]) = progrun x
interpret (Cmd prgs) = runProgsPipe prgs

try' :: IO a ->  IO (Either IOException a)
try' =  try 


runProgsPipe :: [Prg] -> IO ()
runProgsPipe ((IPrg (Ident p) args):prgs) = do
  let arguments = map getArg args
  let process = (proc p arguments){std_out = CreatePipe}
  res <- try' $ createProcess process
  case res of
    Left ex -> print ex
    Right (_,Just so,_,phandle) -> do 
      runProgsPipe' prgs (UseHandle so)
      void $ waitForProcess phandle
    Right (_,Nothing,_,_) -> do
      error "how did i get here"
  return ()


runProgsPipe' :: [Prg] -> StdStream -> IO ()
runProgsPipe' [(IPrg (Ident p) args)] inpipe = do
  let arguments = map getArg args
  let process = (proc p arguments){std_in = inpipe}
  res <- try' $ createProcess process
  case res of
    Left ex -> print ex
    Right (_,_,_,phandle) -> void $ waitForProcess phandle
  return ()

runProgsPipe' ((IPrg (Ident p) args):prgs) inpipe = do
  let arguments = map getArg args
  let process = (proc p arguments){std_in = inpipe,
                                   std_out = CreatePipe}
  res <- try' $ createProcess process
  case res of
    Left ex -> print ex
    Right (_,Just so,_,phandle) -> do 
      runProgsPipe' prgs (UseHandle so)
      void $ waitForProcess phandle
  return ()


progrun :: Prg -> IO ()
progrun (IPrg (Ident "cd") args) = do 
    let arguments = map getArg args
    homedir <- getHomeDirectory
    case arguments of
        [] -> setCurrentDirectory homedir
        [x] -> setCurrentDirectory x
        (x:xs) -> putStrLn "cd only takes 1 argument"
progrun (IPrg (Ident p) args) = do 
    let arguments = map getArg args
    res <- try' $ createProcess (proc p arguments)
    case res of 
        Left ex -> print ex
        Right (_,_,_,phandle) -> void $ waitForProcess phandle
    return ()




getArg :: Arg -> String
getArg (NArg arg) = arg
getArg (IArg (Ident (arg))) = arg