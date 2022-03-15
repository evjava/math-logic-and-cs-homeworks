
module Board where

type Pos = (Int, Int)
type Delta = (Int, Int)
type Size = Int

isValid :: Pos -> Size -> Bool
isValid (x, y) size = inBoardRange x && inBoardRange y
  where inBoardRange :: Int -> Bool
        inBoardRange i = (0 <= i) && (i < size)

plus :: Delta -> Pos -> Pos
plus (dx, dy) (x, y) = (x + dx, y + dy)

genLine :: Pos -> Delta -> Size -> [Pos]
genLine pos d size = case (isValid pos size) of
  True -> pos:(genLine (plus d pos) d size)
  False -> []

genVerticalLines :: Size -> [[Pos]]
genVerticalLines size = map (\i -> genLine (i, 0) (0, 1) size) [0..(size - 1)]

genHorizontalLines :: Size -> [[Pos]]
genHorizontalLines size = map (\i -> genLine (0, i) (1, 0) size) [0..(size - 1)]

genStrightLines :: Size -> [[Pos]]
genStrightLines size = (genVerticalLines size) ++ (genHorizontalLines size)

-- diagonal with alpha = pi/4
genDiagLines0 :: Size -> [[Pos]]
genDiagLines0 size = lines1 ++ lines2
  where
    lines1 = map (\i -> genLine (i, 0) (1, 1) size) [0..(size - 2)]
    lines2 = map (\i -> genLine (0, i) (1, 1) size) [1..(size - 2)]

-- diagonal with alpha = 3*pi/4
genDiagLines1 :: Size -> [[Pos]]
genDiagLines1 size = lines1 ++ lines2
  where
    lines1 = map (\i -> genLine (i       , 0) (-1, 1) size) [1..(size - 1)]
    lines2 = map (\i -> genLine (size - 1, i) (-1, 1) size) [1..(size - 2)]

genDiagLines :: Size -> [[Pos]]
genDiagLines size = (genDiagLines0 size) ++ (genDiagLines1 size)
