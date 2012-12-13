module CustomIOandParser where

import System.IO.Unsafe
import Data.String
import Data.Char
import Data.Typeable
import Carneades

-- Basic splitting of the string and dropping comments is done here:

-- This is used for splitting the string into a list of strings so it 
-- can be easily parsed later. Done for convenience.

split :: String -> [String]
split [] = []
split (c:cs)
           | c == '['      = "[": rest
           | c == ']'      = "]": rest
           | c == ' '      = "" : rest
           | c == '.' && not(isDigit $ head cs) = ".": rest
           | c == '\\'     = "" : rest
           | newline_check = "" : rest
           | comment_check = "" : new_string
           | c == ','      = "" : rest
           | head cs == ']'= (c: "") : rest
           | not(isDigit c) && head cs == '.'= (c: ""): rest
           | otherwise     = (c : head rest) : tail rest
           where
               rest          = split cs
               newline_check = c == '\n'
               comment_check = c == '-' && head cs == '-'
               new_string    = split $ dropComment cs

-- Presently not used. Could be potentially useful

splitOn :: String -> Char -> [String]
splitOn [] delim = [""]
splitOn (c:cs) delim
   | c == delim = "" : rest
   | otherwise = (c : head rest) : tail rest
   where
       rest = splitOn cs delim

-- What this function does is removing all the comments from the input
-- and removes until the occurence of a newline, which would mean that
-- the comment is over.

dropComment :: String -> String
dropComment str = dropWhile (/= '\n') str

-------------------------------------------------------------------------------
-- IO Bit. Using unsafeRead for now.

readFromFile:: FilePath -> [String]
readFromFile x = (filter (/= "") $ split $ (unsafeRead x ++ " "))

unsafeRead :: FilePath -> String
unsafeRead x = unsafePerformIO $ readFile x

-------------------------------------------------------------------------------

-- Splitting the list of strings returned by split into list of lists. Each
-- individual sublist contains all the information needed to construct an 
-- argument later.

splitFurther:: [String] -> [[String]]
splitFurther [] = [[]]
splitFurther xs = takeStringTo xs :(splitFurther $ tail $ dropWhile(/=".") xs)

-- This tiny function takes a list of strings till the first dot encountered. 
-- Used in the above function only.

takeStringTo :: [String] -> [String]
takeStringTo xs = takeWhile (/=".") xs

-- Variables used elewhere. Pretty self-explnatory. Split the file onto three
-- chunks and extract all the relevant bits used later.

argStrings :: FilePath -> [[String]]
argStrings x = splitFurther $ takeWhile (/="ASSUMPTIONS:") $ dropWhile(=="ARGUMENTS:") $ readFromFile x

assumptionsString :: FilePath -> [String]
assumptionsString x = takeStringTo $ dropWhile(=="ASSUMPTIONS:") $ dropWhile(/="ASSUMPTIONS:") $ readFromFile x

standardStrings :: FilePath -> [[String]]
standardStrings x = reverse $ tail $ reverse $ splitFurther $ dropWhile(=="STANDARDS:") $ dropWhile(/="STANDARDS:") $ readFromFile x


-------------------------------------------------------------------------------

-- Here starts the parsing bit.


-- As the name suggests this is the main parsing function that would return a
-- list of the arguments used for constructing the arg graph later.

mainParsingFunction :: [[String]] -> [Carneades.Argument]
mainParsingFunction [[]] = []
mainParsingFunction (x:xs) = createArgument x : mainParsingFunction xs

-- This is just a tiny wrapper around mkAssumptions which would do the same
-- thing as mkProp but on a list.

makePropositions :: [String] -> [Proposition]
makePropositions (x:xs) = Carneades.mkAssumptions $ takeWhile (/="]") xs

-- Here is where the argument is actually created.
createArgument :: [String] -> Carneades.Argument
createArgument (x:xs) = Arg (makePropositions xs) (makePropositions $ tail $ dropWhile(/="]") xs) (Carneades.mkProp $ head $ tail $ reverse xs)

-- used for testing purposes only. To be removed. Maybe.

makeArguments :: [String] -> [Carneades.Argument]
makeArguments xs = mainParsingFunction $ splitFurther xs

-- Here is where the arg graph needed later by the framework later is constructed.
    
makeArgGraph :: [Carneades.Argument] -> AGraph
makeArgGraph args = Carneades.mkArgGraph(args)


-- In order to construct the weights we need some information that has previously
-- been discarded - such as the arg id, the weight.

listOfArgumentIDs :: [[String]] -> [String]
listOfArgumentIDs [[]] = []
listOfArgumentIDs (x:xs) = head x : listOfArgumentIDs xs

listOfWeights :: [[String]] -> [Double]
listOfWeights [[]] = []
listOfWeights (x:xs) = (read (head $ reverse x)::Double): listOfWeights xs

infoTuples :: FilePath -> [(String, Argument, Double)]
infoTuples x = zip3 (listOfArgumentIDs $ argStrings x) (mainParsingFunction $ argStrings x) (listOfWeights $ argStrings x)


-- Here is where the weight is found. Just loops through all the arguments and 
-- finds the correct argument and returns its weight

findArgument :: Argument -> [(String, Argument, Double)] -> Double
findArgument arg args
                        | arg == arg_itself = weight
                        | otherwise = findArgument arg (tail args)
                                where (name, arg_itself, weight) = head(args)

-- Here we're making list for the assumtpions and creating the audience

makeAssumptions :: [String] -> [Proposition]
makeAssumptions xs = makePropositions xs

assumptionsList :: FilePath -> [Proposition]
assumptionsList x = makeAssumptions $ assumptionsString x

makeAudience :: FilePath -> Weight -> Audience
makeAudience x weight = (assumptionsList x, weight)


-- This is where all the standarts are created. 
-- First, I am making a list of the all the propositions and strings.

makeListForStandard :: [[String]] -> [(Proposition, String)]
makeListForStandard [] = []
makeListForStandard (x:xs) = (mkProp $ head(x), head(tail(x))): makeListForStandard xs

-- Here we're taking the filepath and returning a list of the tuples

standardList :: FilePath -> [(Proposition, String)]
standardList x = makeListForStandard $ standardStrings x

-- For ease, I am making a list of tuples of the standarts and their names
-- so I can match them later with the standarts from the file.

listOfAllStandards :: [ProofStandard]
listOfAllStandards = [scintilla, beyond_reasonable_doubt, clear_and_convincing, preponderance, dialectical_validity]

listOfStandardNames :: [String]
listOfStandardNames = ["scintilla", "beyond_reasonable_doubt", "clear_and_convincing", "preponderance", "dialectical_validity"]

standardTuples :: [(ProofStandard, String)]
standardTuples = zip listOfAllStandards listOfStandardNames

-- This is just finding the standart and looping through both lists and returns
-- a standart when it finds a match.

findStandard :: Proposition -> [(Proposition, String)] -> ProofStandard
findStandard propy [] = scintilla
findStandard propy (x:xs) | propy == proposition = matchStandardToName standard_name standardTuples
                          | otherwise = findStandard propy xs
                                where (proposition, standard_name) = x
                                       

matchStandardToName :: String -> [(ProofStandard, String)] -> ProofStandard
matchStandardToName strToMatch [] = scintilla
matchStandardToName strToMatch (x:xs) | strToMatch == strFromTuple = standardFromTuple
                                      | otherwise = matchStandardToName strToMatch xs
                                           where (standardFromTuple, strFromTuple) = x

-- And here is the one and only function which give a file path will 
-- return a full CAES which could be used as one would like.

parseFileMakeCAES :: FilePath -> CAES 
parseFileMakeCAES x = CAES (makeArgGraph $ mainParsingFunction $ argStrings x, myAudience, assingsNewStandard)
        where
            myAudience = makeAudience x myWeight
            myWeight arg = findArgument arg $ infoTuples x
            assingsNewStandard prop = findStandard prop $ standardList x 

testProposition :: String -> CAES -> Bool
testProposition string_to_test caes = acceptable (mkProp string_to_test) caes 


