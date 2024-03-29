module Parse (parse, Command(..), Program(..)) where

import GRM.Abs


data Command = Background [Program] | Foreground [Program]
      deriving (Show, Eq)
      
data Program = Prog String [String] 
      deriving (Show, Eq)



parse :: Cmd -> [Command] 
parse (BCmd prgs) = map (Foreground) $ combinations $ map (parseProg) prgs
parse (FCmd prgs) = map (Foreground) $ combinations $ map (parseProg) prgs






parseProg :: Prg -> [Program]
parseProg (IPrg (Id prog) args) = expandProg prog args
parseProg (SPrg prog args) = expandProg prog args

-- Expands a single Prg into its respective programs
expandProg :: String -> [Arg] -> [Program]
expandProg ex args = [(Prog ex xs) |  xs <- combos]
    where expand = map expandArg args :: [[String]]
          combos = combinations expand



combinations :: [[a]] -> [[a]]
combinations []       = [[]]
combinations (xs:xss) = [x : xs' | x <- xs, xs' <- combinations xss]



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
