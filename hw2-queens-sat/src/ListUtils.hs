
module ListUtils where

sublis :: Int -> Int -> [a] -> [a]
-- (a: inclusively, b: exclusively)
sublis a b list = take (b - a) (drop a list)

popAt :: [a] -> Int -> (a, [a])
popAt list i
  | 0 <= i && i < len = (list !! i, (sublis 0 i list) ++ (sublis (i + 1) len list))
  | otherwise         = error $ "Unexpected pos = " ++ (show i) ++ "expected in (0, " ++ (show len) ++ ")"
  where
    len = length list

popAtAll :: [a] -> [(a, [a])]
popAtAll list = map (popAt list) (indices list)
  where
    indices :: [a] -> [Int]
    indices l = [0..((length l) - 1)]
