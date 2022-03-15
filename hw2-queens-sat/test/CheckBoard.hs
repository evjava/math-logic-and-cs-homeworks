
module CheckBoard where

import Board

assert :: String -> Bool -> Int
assert msg cond =
  case cond of
    True -> 0
    False -> error msg



main :: IO ()
main = do
  let res = assert "genLine FAILED!" $ genLine (0, 0) (1, 0) 2 == [(0, 0), (1, 0)]
  if res /= 0
    then error "FAILED!"
    else putStrLn ".."
