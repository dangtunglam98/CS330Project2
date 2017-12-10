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

editOnce :: String ->[String]
editOnce word = (addition word) ++ (deletion word) ++ (subtitution word)

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


--Import File into List
getWord :: FilePath -> IO [String]
getWord path = do
	contents <- readFile path
	return (lines contents)

--SpellCheck
sameElem :: String -> FilePath -> [String]
sameElem word file = [x | x <- editOnce word, y <- unsafePerformIO $ getWord file , x == y ]
