import Data.List
import System.IO.Unsafe
import Data.Char
import Data.List.Split

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
editOnce word = (addition word) ++ (deletion word) ++ (subtitution word)

editNextStep :: [String] -> [String]
editNextStep ls = [ x | e <- ls, x <- (editOnce e) ]

removePunc :: String -> String
removePunc str = [x | x <- str, not (x `elem` punc)]
	where punc = ",.?!:;\"\'#$%"

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
	let split = splitOn " " (removePunc (toLowerString (internalHandle contents)))
	let ls = [ x | x <- split, not (wordNoLetters x) ]
	return ls


--Import Dictionary into List
getDiction :: FilePath -> IO [String]
getDiction path = do
	contents <- readFile path
	return (lines contents)

--SpellCheck
sameElem :: String -> FilePath -> [String]
sameElem word file = [x | x <- editOnce word, y <- unsafePerformIO $ getDiction file , x == y ]

getBestTen :: [String] -> [String] -> [String]
getBestTen wordls dict
	| (length [x | x <- wordls, x `elem` dict]) < 10 = getBestTen (editNextStep wordls) dict
	| otherwise = take 10 [x | x <- wordls, x `elem` dict]

checkFile dict mis result = do
	diction <- getDiction dict
	contents <- handleInput mis
	let needFix = [x | x <- contents, not (x `elem` diction)]
	let fixed = [unwords (x : ":" : (getBestTen (editOnce x) diction)) | x <- needFix]
	writeFile result (unlines fixed)
