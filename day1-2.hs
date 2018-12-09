import qualified Data.Set as S

main = interact process

process :: String -> String
process = show . loop 0 (S.singleton 0) . cycle . map parse . lines

parse :: String -> (Integer -> Integer)
parse ('+':rest) = (+) $ read rest
parse ('-':rest) = (subtract) $ read rest

loop :: Integer -> S.Set Integer -> [(Integer -> Integer)] -> Integer
loop curr seen (fn:rest) =
    if n `S.member` seen
        then n
        else loop n (n `S.insert` seen) rest
    where
        n = fn curr
