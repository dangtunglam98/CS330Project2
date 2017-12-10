import Data.List
import System.IO.Unsafe
import Data.Char
import Data.List.Split
import qualified Data.Set as Set
--Addition,Subtitution,Deletions
addition :: String -> [String]
addition word = [left ++ c: right | (left,right) <- splits word, c <- letters]
	where letters = ['a'..'z']

deletion :: String -> [String]
deletion word = [left ++ tail right | (left,right) <- splits word, (not . null) right]

subtitution :: String -> [String]
subtitution [] = []
subtitution word = [left ++ c:tail right |(left,right) <- splits word, (not . null) right, c <- letters]
	where letters = ['a'..'z']

splits :: String -> [(String,String)]
splits word = zip (inits word) (tails word)

editOnce :: String -> [String]
editOnce word = (subtitution word) ++ (addition word) ++ (deletion word)

editNextStep :: [String] -> [String]
editNextStep ls = [ x | e <- ls, x <- (editOnce e) ]

removePunc :: String -> String
removePunc str = [x | x <- str, not (x `elem` punc)]
	where punc = ",.?!:;\"\'#$%\n"

internalHandle :: String -> String
internalHandle str = [if (x == '-') || (x == '&') then ' ' else x | x <- str]

wordNoLetters :: String -> Bool
wordNoLetters "" = True
wordNoLetters str = (not (isAlpha (head str))) && (wordNoLetters (tail str))

toLowerString :: String -> String
toLowerString str = [if isAlpha x then toLower x else x | x <- str]

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
sameElem :: [String] -> FilePath -> [String]
sameElem words file = [x | x <- words, y <- unsafePerformIO $ getDiction file , x == y ]


getBestTen :: String -> FilePath -> [String]
getBestTen word dict = 	take 10 alternatives
	where alternatives = sameElem [word] dict ++ sameElem (editOnce word) dict ++ [word]



checkInterSection10 :: [String] -> [String] -> Set.Set String
checkInterSection10 mis dic = Set.take 10 (Set.intersection (Set.fromList mis) (Set.fromList dic ))

checkFile dict mis result = do
	contents <- handleInput mis
	diction <- getDiction dict
	--let needFix = [x | x <- contents, not (Set.isSubsetOf (Set.fromList [x]) (Set.fromList diction))]
	let needFix = [x | x <- contents, not (x `elem` diction)]
	let fixed = [unwords (x : ":" : (Set.toList ((checkInterSection10 (editNextStep (editOnce x)) diction)))) | x <- needFix ]
	writeFile result (unlines fixed)
