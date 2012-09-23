module LifeV where
import Data.List (sort, group)
import qualified Data.Vector as V
import Control.Arrow ((&&&))

fPent :: Board
fPent = V.fromList $ [(1, 2), (2, 2), (2, 1), (2, 3), (3, 3)]

type Edge = (Int, Int)
type Cell = (Int, Int)
type Board = V.Vector Cell

life :: Board -> Board
life cells = V.map snd . V.filter rules . freq $ V.concatMap neighbours cells
    where rules (n, c) = n == 3 || (n == 2 && c `V.elem` cells)
          freq = V.fromList . map (length &&& head) . group . sort . V.toList

neighbours :: Cell -> V.Vector Cell
neighbours (x, y) = V.fromList $ [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0]
