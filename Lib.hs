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
import System.Console.Haskeline
import Control.Concurrent
import System.Process.Internals (GroupID(..))

setup :: IO ()
setup = do
  hSetBuffering stdout NoBuffering
  void $ installHandler sigINT Ignore Nothing



someFunc :: IO ()
someFunc = runInputT defaultSettings loop
  where
  loop :: InputT IO ()
  loop = do
      str <- liftIO getCurrentDirectory
      let prmpt = (str ++ " > ")
      minput <- getInputLine prmpt
      case minput of
          Nothing -> return ()
          Just input -> do  case pCmd (myLexer input) of
                              Ok t -> liftIO $ interpret t
                              Bad err -> liftIO $ putStrLn err
                            loop



interpret :: Cmd -> IO ()
interpret (BCmd []) = return ()
interpret (FCmd []) = return ()
interpret cmd = mapM_ runProgsPipe prs
    where prs = parse cmd

try' :: IO a ->  IO (Either IOException a)
try' =  try


runProgsPipe :: Command -> IO ()
runProgsPipe (Foreground prgs@(Prog exec args : rest)) = foregroundPipe prgs 0 (UseHandle stdin)
runProgsPipe (Background prgs) = do
  forkIO $ foregroundPipe prgs 0 CreatePipe
  return ()


foregroundPipe :: [Program] -> GroupID -> StdStream -> IO ()
foregroundPipe [Prog "cd" args] _ inpipe = do 
  homedir <- getHomeDirectory
  case args of
    [] -> setCurrentDirectory homedir
    [x] -> setCurrentDirectory x
    (x:xs) -> putStrLn "cd: only takes one argument"
foregroundPipe [Prog exec args] grpid inpipe = do
  let process = case grpid of 
                  0 -> (proc exec args){std_in = inpipe, create_group = True }
                  i -> (proc exec args){std_in = inpipe, child_group = Just i }
  res <- try' $ createProcess process
  case res of
    Left ex -> print ex
    Right (_,_,_,phandle) -> void $ waitForProcess phandle
  print "Done"
  return ()
foregroundPipe (Prog "cd" args : rest) grpid inpipe = do 
  homedir <- getHomeDirectory
  case args of
    [] -> setCurrentDirectory homedir
    [x] -> setCurrentDirectory x
    (x:xs) -> putStrLn "cd: only takes one argument"
  foregroundPipe rest grpid CreatePipe
foregroundPipe (Prog exec args : rest) grpid inpipe = do
  let process = case grpid of 
                  0 -> (proc exec args){std_in = inpipe, create_group = True }
                  i -> (proc exec args){std_in = inpipe, child_group = Just i }  
  res <- try' $ createProcess process
  case res of
    Left ex -> print ex
    Right (_,Just so,_,phandle) -> do
      foregroundPipe rest (getGid)(UseHandle so)
      void $ waitForProcess phandle
  return ()
