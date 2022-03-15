
module CheckListUtils where

import ListUtils

assert :: String -> Bool -> Int
assert msg cond =
  case cond of
    True -> 0
    False -> error msg

checkSublis :: (Int, Int, [Int]) -> [Int] -> Int
checkSublis inp@(a, b, li) expected =
  assert ("failed sublis on " ++ (show inp) ++ ";\n\texpected: " ++ (show expected) ++ ";\n\tactual: " ++ (show actual)) (actual == expected)
    where actual = sublis a b li

checkSublisAll :: IO()
checkSublisAll = do
  putStrLn $ if (res == 0) then "checkSublisAll: PASSED!" else "FAILED!"
    where
      li = [0, 1, 2, 3, 4, 5]
      variants =
        [ ((0, 3, li), [0, 1, 2])
        , ((1, 4, li), [1, 2, 3])
        , ((0, 6, li), li)
        ]
      res0 = map (\(input, expected) -> checkSublis input expected) variants
      res = sum res0
  
checkPopAt :: IO()
checkPopAt = do
  putStrLn $ if (res == 0) then "checkPopAt: PASSED!" else "FAILED!"
    where
      li = [0, 1, 2]
      variants =
        [ (([1, 2, 3], 0), (1, [2,3]))
        , (([1, 2, 3], 1), (2, [1,3]))
        , (([1, 2, 3], 2), (3, [1,2]))
        ]
      res0 = map (\((ar, i), expected) -> if ((popAt ar i) == expected) then 0 else 1) variants
      res = sum res0
                 

main :: IO ()
main = do
  checkSublisAll
  checkPopAt
