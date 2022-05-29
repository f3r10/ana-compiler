module AnaCompiler.Utils (findPos, lookUp) where

findPos :: (Eq b1, Num b2, Enum b2) => [b1] -> b1 -> [b2]
findPos list elt = map fst $ filter ((elt==).snd) $ zip [0..] list


lookUp :: [String] -> String -> Maybe Int
lookUp [] _ = Nothing
lookUp (x:xs) s | not(x == s)  = fmap (1 +) (lookUp xs s)
                | otherwise    = Just 0

