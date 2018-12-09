main = interact $ show . process . lines

process :: [String] -> Integer
process lines =
    let
        tmp1 = countPred (containsExactCountOfAny 2) lines
        tmp2 = countPred (containsExactCountOfAny 3) lines
    in
        tmp1 * tmp2

containsExactCountOfAny :: Integer -> String -> Bool
containsExactCountOfAny n str = any (\c -> hasExactCount n c str) str

hasExactCount :: Integer -> Char -> String -> Bool
hasExactCount n c str = count c str == n

count :: Eq a => a -> [a] -> Integer
count a = countPred (==a)

countPred :: (a -> Bool) -> [a] -> Integer
countPred pred = sum . map (const 1) . filter pred
