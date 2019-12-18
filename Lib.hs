module Lib
    ( someFunc,
      setup
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
import System.Posix.Signals
import Parse (parse, Command(..), Program(..))


setup :: IO ()
setup = do
  hSetBuffering stdout NoBuffering
  void $ installHandler sigINT Ignore Nothing


someFunc :: IO ()
someFunc = do
  str <- getCurrentDirectory
  putStr (str ++ " > ")
  cmdstr <- getLine
  case pCmd (myLexer cmdstr) of
          Ok t -> interpret t
          Bad err -> putStrLn err
  someFunc


interpret :: Cmd -> IO ()
interpret cmd = mapM_ runProgsPipe prs
    where prs = parse cmd

try' :: IO a ->  IO (Either IOException a)
try' =  try 


runProgsPipe :: Command -> IO ()
runProgsPipe (Foreground prgs@(Prog exec args : rest)) = foregroundPipe prgs (UseHandle stdin) 
runProgsPipe (Background prgs) = backgroundPipe prgs


foregroundPipe :: [Program] -> StdStream -> IO ()
foregroundPipe [Prog exec args] inpipe = do
  let process = (proc exec args){std_in = inpipe}
  res <- try' $ createProcess process
  case res of
    Left ex -> print ex
    Right (_,_,_,phandle) -> void $ waitForProcess phandle
  return ()
foregroundPipe (Prog exec args : rest) inpipe = do
  let process = (proc exec args){std_in = inpipe, std_out = CreatePipe}
  res <- try' $ createProcess process
  case res of
    Left ex -> print ex
    Right (_,Just so,_,phandle) -> do 
      foregroundPipe rest (UseHandle so)
      void $ waitForProcess phandle
  return ()

backgroundPipe :: [Program] -> IO ()
backgroundPipe [Prog exec args] = do
  let process = (exec args)
  res <- try' $ spawnProcess process
  return ()