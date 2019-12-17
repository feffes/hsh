module Parse where

import GRM.Abs
import Lib (Command, Program)

parse :: Cmd -> [Command] 
--parse (BCmd prgs) = 
parse (FCmd prgs) = undefined




parseProg :: Prg -> [Program]
parseProg (IPrg (Id prog) args) = expandProg prog args
parseProg (SPrg prog args) = expandProg prog args

-- Expands a single Prg into its respective programs
expandProg :: String -> [Arg] -> [Program]
expandProg ex args = undefined
    where expand = map expandArg args :: [[String]]



combinations :: [[a]] -> [[a]]
combinations []       = [[]]
combinations (xs:xss) = [x : xs' | x <- xs, xs' <- combinations xss]


{-
[1,2] -> ["1","2"]
[s]     -> [s]

Prg "ls" []


ls {1,2} s -> [["1", s], ["2", s]] = stuff
= ls stuff!!0
    ls stuff!!1

-}

expandArg :: Arg -> [String]
expandArg (NArg str) = [str]
expandArg (IArg (Id str)) = [str]
expandArg (LArg strs) = strs
expandArg (RArg low high) = final
    where   range =  [low..high]
            irange = [high..low]
            final = case length range of
                0 -> case length irange of
                    0 -> error "how did i get here"
                    i -> map show $ reverse irange 
                i -> map show range
