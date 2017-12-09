
--Addition,Subtitution,Deletions
addition :: [Char] -> [Char] -> [[Char]]
addition letters word = 
	do
		l <- letters
		return (l:word)

deletion :: [Char] -> [Char]
deletion [] = []
deletion (_:xs) = xs

subtitution :: [Char] -> [Char] -> [Char]
subtitution _ [] = []
subtitution letters (x:xs) = addition letters xs

splits :: [Char] -> [([Char],[Char])]
splits word = zip (inits word) (tails word)





--Import File into List
getWord :: FilePath -> IO [String]
getWord path = do 
	contents <- readFile path
	return (lines contents)