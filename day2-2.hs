main = interact $ process . lines

process :: [String] -> String
process = sameElements . head . filter ((==1) . similarityIndex) . everyPair

everyPair :: [a] -> [(a, a)]
everyPair [] = []
everyPair (a:as) = map (\b -> (a,b)) as ++ everyPair as

countPred :: (a -> Bool) -> [a] -> Integer
countPred pred = sum . map (const 1) . filter pred

similarityIndex :: Eq a => ([a], [a]) -> Integer
similarityIndex (a, b) = countPred (uncurry (/=)) $ zip a b

sameElements :: Eq a => ([a], [a]) -> [a]
sameElements (a, b) = map fst . filter (uncurry (==)) $ zip a b
