import qualified CheckListUtils
import qualified CheckNormalizer
import qualified CheckBoard

main :: IO ()
main = do
  CheckNormalizer.main
  putStrLn "Normalizer: PASSED!"
  CheckListUtils.main
  putStrLn "ListUtils: PASSED!"
  CheckBoard.main
  putStrLn "Board: PASSED!"
