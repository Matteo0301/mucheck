module Main where
import System.Environment

import MuCheck.MuOp
import Language.Haskell.Exts
import MuCheck.StdArgs
import MuCheck.Interpreter
import MuCheck.Mutation
import MuCheck.Operators
import MuCheck.Utils.Common

process :: String -> String -> String -> [String] -> IO ()
process fn file modulename args = do
  numMutants <- genMutants fn file
  checkPropsOnMutants (take numMutants $ genFileNames file) modulename args "./test.log"
  return ()


main :: IO ()
main = do
  val <- getArgs
  case val of
    ("-h" : _ ) -> help
    (fn : file : modulename : args) -> process fn file modulename args
    _ -> error "Need function file modulename [args]\n\tUse -h to get help"

help :: IO ()
help = putStrLn ("mucheck function file modulename [args]\n" ++
       "E.g: ./mucheck qsort Examples/Quicksort.hs Examples.Quicksort 'quickCheckResult idEmpProp' 'quickCheckResult revProp' 'quickCheckResult modelProp'")

