
--Addition,Subtitution,Deletions
addition :: [String] -> [String] -> [String]
addition letters word = 
	do
		l <- letters
		return (l:word)

deletion :: [String] -> [String]
deletion [] = []
deletion (_:xs) = xs

subtitution :: [String] -> [String] -> [String]
subtitution _ [] = []
subtitution letters (x:xs) = addition letters xs 