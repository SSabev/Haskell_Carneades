module BurdenOfProofSimulation where

import Data.String
import Data.Char
import Data.Typeable
import Data.List
import CustomIOandParser
import Carneades
import System.IO

-- for testing
argumentGraph :: FilePath -> AGraph
argumentGraph x = makeArgGraph $ args x

args :: FilePath -> [Argument]
args x = mainParsingFunction $ argStrings x

-- get all the propositions to be used by one party
getConProps :: [Argument] -> [Proposition]
getConProps [] = []
getConProps ((Arg x y z):xs) | fst(z) = x ++ getConProps xs 
                             | otherwise = y ++ getConProps xs

-- propositions for the other party
getDefProps :: [Argument] -> [Proposition]
getDefProps [] = []
getDefProps ((Arg x y z):xs) | not(fst(z)) = x ++ getDefProps xs 
                             | otherwise = y ++ getDefProps xs

-- get suitable counterarguments
--
generatePropTuples :: [Argument] -> [([Proposition], [Proposition])]
generatePropTuples [] = []
generatePropTuples ((Arg x y z):xs) | fst(z) = (x, y) : generatePropTuples xs
                                    | otherwise = (y,x) : generatePropTuples xs

getAllArgTuple :: FilePath -> [([Proposition], [Proposition])]
getAllArgTuple fp = generatePropTuples $ args fp

-- Get some of the assumptions for generating the initial CAES
getCustomAssumptions :: FilePath -> Int -> [Proposition]
getCustomAssumptions x num = take num (getConProps $ args x) ++ take num (getDefProps $ args x) 

-- used for constructing a custom audience, which is needed as we are essentially
-- creating a fair few CAESs as we go along the way.

makeCustomAudience :: [Proposition] -> Weight -> Audience
makeCustomAudience assumptions weight = (assumptions, weight)

makeCAES :: FilePath -> [Proposition] -> CAES 
makeCAES x props = CAES (argumentGraph x, myAudience, assingsNewStandard)
        where
            myAudience = makeCustomAudience props customWeight
            customWeight arg = findArgument arg (infoTuples x)
            assingsNewStandard prop = findStandard prop $ standardList x

maxNumberOfTries :: FilePath -> Int
maxNumberOfTries x = length(getConProps $ args x)

-- This is where the magic happens for the defense. It's not a really elegant
-- solution, but giving the framework itself is not elegant, this is quite good!

defendSuccessfully :: [([Proposition], [Proposition])] -> [Proposition] -> [Proposition] -> Proposition
defendSuccessfully [] _ leftToUse = head leftToUse
defendSuccessfully ((pro,con):xs) lastAccusedOf leftToUse 
        | accusation `elem` pro && defensiveArg `elem` leftToUse = defensiveArg
        | accusation `elem` pro && not (defensiveArg `elem` leftToUse) = defendSuccessfully ((pro, tail con):xs) lastAccusedOf leftToUse
        | otherwise = defendSuccessfully xs lastAccusedOf leftToUse 
            where 
                accusation = head lastAccusedOf
                defensiveArg = head con       

-- This is the function that simulate the whole trial. Not the pretties one
-- you'll see, yes, but it works.

prettyHelper :: [Proposition] -> [String]
prettyHelper [] = []
prettyHelper (x:xs) = (snd x):prettyHelper xs

trialSimulation :: FilePath -> [Proposition] -> [Proposition] -> [Proposition] -> [Proposition] -> (String,Bool) -> IO ()
trialSimulation fp currAssumpt defAssumpt accAssumpt assumpLeft (blamedFor,result)
     | currAssumpt == [] = do
          putStrLn initialMessage
          putStrLn choseArgMessage
          putStrLn ("Current assumptions are: " ++ show (initialAssumptions))
          funcPointer <- trialSimulation fp ((head accAssumpt):currAssumpt) defAssumpt (tail accAssumpt) (delete (head accAssumpt) assumpLeft) (blamedFor, new_result)
          return funcPointer
    | not (result) && length(accAssumpt)/=0  && length(assumpLeft) /= 0 = do
          putStrLn ("Currently the result for " ++ blamedFor ++ " is " ++ show result ++ " and the length of the list is " ++ show (length(accAssumpt)))
          putStrLn whoIsNext
          putStrLn ("Current assumptions are: " ++ show (currAssumptions))
          putStrLn argChosenToProveTrue
          funcPointer <- trialSimulation fp ((head accAssumpt):currAssumpt) defAssumpt (tail accAssumpt) (delete (head accAssumpt) assumpLeft) (blamedFor, new_result)
          return funcPointer
    | result  && length(defAssumpt)/=0 && length(assumpLeft) /= 0 = do
          putStrLn ("Currently the case is considered " ++ show result)
          putStrLn whoIsNext
          putStrLn argChosen
          putStrLn ("Current assumptions are: " ++ show (currAssumptions))
          funcPointer <- trialSimulation fp (chosenDefProp:currAssumpt) (delete chosenDefProp defAssumpt) (accAssumpt) (delete chosenDefProp assumpLeft) (blamedFor, new_result)
          return funcPointer
    | otherwise = putStrLn ("Final verdict for the subject being judged for " ++ blamedFor ++ " is " ++ show result)
        where
            whoIsNext = pickSide result
            argChosen = "Trying to disprove that " ++ blamedFor ++  " is " ++ show result ++ ". Chosen the following element: " ++ snd chosenDefProp ++ " from the evidence."
            argChosenToProveTrue = "The party has chosen the following element: " ++ chosenArg ++ " from the evidence"
            initialMessage = "Starting trial. Ultimately we're trying to prove " ++ blamedFor
            choseArgMessage= "The defense has chosen: " ++ chosenArg ++ " from the list of available evidence."
            new_result = testProposition blamedFor caes
            caes = makeCAES fp currAssumpt
            chosenArg = snd $ head accAssumpt
            chosenDefProp = defendSuccessfully (getAllArgTuple fp) currAssumpt defAssumpt
            initialAssumptions = prettyHelper [head accAssumpt]
            currAssumptions = prettyHelper currAssumpt

-- This is a simple function which is just used for determining which of the 
-- messages to display.
pickSide :: Bool -> String
pickSide x  | x = "The defense is now trying to defeat the arguments:"
            | otherwise = "The accusation is now presenting arguments:"

-- This function initiates the whole process. We initiate it with a list of
-- empty assumptions and start building as we go along taking both from the 
-- 
initiateSimulation :: FilePath -> String -> IO()
initiateSimulation fp prop = trialSimulation fp [] defense accusations (accusations++defense) (prop,False)
    where
    defense = getDefProps $ args fp
    accusations = getConProps $ args fp
