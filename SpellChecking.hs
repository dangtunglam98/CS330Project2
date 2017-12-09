import Data.List
import System.IO.Unsafe
--Addition,Subtitution,Deletions
addition :: [Char] -> [[Char]]
addition word = [left ++ c: right | (left,right) <- splits word, c <- letters]
	where letters = ['a'..'z']
	

deletion :: [Char] -> [[Char]]
deletion word = [left ++ tail right | (left,right) <- splits word, (not . null) right]

subtitution :: [Char] -> [[Char]]
subtitution [] = []
subtitution word = [left ++ c:tail right |(left,right) <- splits word, (not . null) right, c <- letters]
	where letters = ['a'..'z']

splits :: [Char] -> [([Char],[Char])]
splits word = zip (inits word) (tails word)

editOnce :: [Char] ->[[Char]]
editOnce word = (addition word) ++ (deletion word) ++ (subtitution word)


--Import File into List
getWord :: FilePath -> IO [String]
getWord path = do 
	contents <- readFile path
	return (lines contents)

--SpellCheck
sameElem :: [Char] -> FilePath -> [[Char]]
sameElem word file = [x | x <- editOnce word, y <- unsafePerformIO $ getWord file , x == y ]