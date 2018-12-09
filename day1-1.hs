main = interact process

process :: String -> String
process = show . foldr ($) 0 . map parse . lines

parse :: String -> (Integer -> Integer)
parse ('+':rest) = (+) $ read rest
parse ('-':rest) = (subtract) $ read rest
