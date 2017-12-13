import Data.List
import System.IO.Unsafe
import Data.Char
import Data.List.Split
import qualified Data.Set as Set
--Addition,Subtitution,Deletions
addition :: String -> [String] -- Return a list of the word added a letter
addition word = [left ++ c: right | (left,right) <- splits word, c <- letters]
	where letters = ['a'..'z']

deletion :: String -> [String] -- Return a list of the word deleted a letter
deletion word = [left ++ tail right | (left,right) <- splits word, (not . null) right]

subtitution :: String -> [String] -- Return a list of the word subtituted a letter
subtitution [] = []
subtitution word = [left ++ c:tail right |(left,right) <- splits word, (not . null) right, c <- letters]
	where letters = ['a'..'z']

splits :: String -> [(String,String)] -- Return a list of multiple components of the word
splits word = zip (inits word) (tails word)

--Return a list of editted versions of a word
editOnce :: String -> [String]  
editOnce word = (subtitution word) ++ (addition word) ++ (deletion word)

editNextStep :: [String] -> [String]
editNextStep ls = [ x | e <- ls, x <- (editOnce e) ]

-- Handle input file
removePunc :: String -> String -- Return the word with no punctuation
removePunc str = [x | x <- str, not (x `elem` punc)]
	where punc = ",.?!:;\"\'#$%\n"

internalHandle :: String -> String -- Return a word that has been handled with special character
internalHandle str = [if (x == '-') || (x == '&') then ' ' else x | x <- str]

wordNoLetters :: String -> Bool -- Check if the String is valid
wordNoLetters "" = True
wordNoLetters str = (not (isAlpha (head str))) && (wordNoLetters (tail str))

toLowerString :: String -> String -- Return the lowercase version of the word
toLowerString str = [if isAlpha x then toLower x else x | x <- str]

-- Import test file into a List
handleInput :: FilePath -> IO [String]
handleInput path = do
	contents <- readFile path
	let split = splitOn " " (toLowerString (internalHandle (removePunc contents)))
	let ls = [ x | x <- split, not (wordNoLetters x) ]
	return ls

--Import Dictionary into List
getDiction :: FilePath -> IO [String]
getDiction path = do
	contents <- readFile path
	return (lines contents)

--SpellCheck

getInterSection10 :: [String] -> [String] -> Set.Set String
getInterSection10 mis dic = Set.take 10 (Set.intersection (Set.fromList mis) (Set.fromList dic ))

spellCheck dict mis result = do -- get dictionary file , mispelled file and name of output file
	contents <- handleInput mis
	diction <- getDiction dict
	let needFix = [x | x <- contents, not (x `elem` diction)] --get words that need fixing
	let fixed = [unwords (x : ":" : (Set.toList ((getInterSection10 (editNextStep (editOnce x)) diction)))) | x <- needFix ] -- Return mispelled word along with a list of correct spelling of the word
	writeFile result (unlines fixed) -- Write it into a file
