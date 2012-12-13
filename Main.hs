module Main(main) where

import System.Environment
import BurdenOfProofSimulation

main :: IO ()
main = do
  sysargs <- System.Environment.getArgs
  funcPointer <- triggerProcess sysargs
  return funcPointer
    
triggerProcess :: [String] -> IO()
triggerProcess sysargs = initiateSimulation fileName proposition_to_test
    where 
        proposition_to_test = head $ tail sysargs
        fileName = head sysargs
