module Life where
import Data.List (group, sort, concatMap)
import Control.Arrow ((&&&))

-- test input
fPent :: Board
fPent = [(1, 2), (2, 2), (2, 1), (2, 3), (3, 3)]

-- some simple type wrappers
type Edge  = (Int, Int)
type Cell  = (Int, Int)
type Board = [Cell]

-- life: progresses the current game state one turn according to the rules of Life.
-- Uses neighbours to create a list containing all Cells which
-- neighbour a live Cell, and counts the number of occurences of each Cell.
-- The number of occurrences of a Cell in this list is the number of live
-- Cells neighbouring it, and the Cells which will not progress to the next
-- stage of Life can then be filtered out.
life :: Board -> Board
life cells = map snd . filter rules . freq $ concatMap neighbours cells
    where rules (n, c) = n == 3 || (n == 2 && c `elem` cells)
          freq = map (length &&& head) . group . sort

-- neighbours: creates a list containing all of the neighbours of a given
-- Cell, excluding the Cell itself.
neighbours :: Cell -> [Cell]
neighbours (x, y) = [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0]

-- "wrap" a Cell around the edges of a Board, to simulate an infinite playing surface
wrapCell :: Edge -> Cell -> Cell
wrapCell (w, h) (x, y) = (wrap x w, wrap y h)
    where wrap a b
            | a < 0     = wrap (b + a) b
            | a > b     = wrap (a - b) b
            | otherwise = a

-- apply wrapCell to every Cell in a Board
wrapBoard :: Edge -> Board -> Board
wrapBoard edges = map (wrapCell edges)
