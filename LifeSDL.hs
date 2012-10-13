import Life
import Control.Monad
import Graphics.UI.SDL as SDL
import Data.List (nub)

-- global config options

cellsize = 4
height   = 480
width    = 640
depth    = 32

-- wrap a Cell in the SDL.Rect data type
cellToRect :: Cell -> Rect
cellToRect (x, y) = Rect (cellsize * x) (cellsize * y) cellsize cellsize

-- add a Cell to the screen surface, but dont draw it yet
addCell :: Surface -> Cell -> IO Bool
addCell screen cell = do
    pixel   <- mapRGB (surfaceGetPixelFormat screen) 255 255 255
    fillRect screen (Just $ cellToRect cell) pixel

-- draw a Board to the screen surface
drawBoard :: Surface -> Board -> IO ()
drawBoard screen b = do
    pixel   <- mapRGB (surfaceGetPixelFormat screen) 0 0 0
    fillRect screen Nothing pixel
    mapM_ (addCell screen) b
    SDL.flip screen

-- draw a cursor (grey) to the screen surface
drawCursor :: Surface -> Cell -> IO ()
drawCursor screen cell = do
    pixel   <- mapRGB (surfaceGetPixelFormat screen) 125 125 125
    fillRect screen (Just $ cellToRect cell) pixel
    SDL.flip screen

-- poll for events from the user, like quitting or saving the game state.
-- if no user events occur, continually draws and updates the game state.
gen :: Edge -> Surface -> Board -> IO ()
gen edges screen board = do
    quit    <- whileEvents
    drawBoard screen $ wrapBoard edges board
    unless quit (gen edges screen $ life board)
    where whileEvents = do
              event     <- pollEvent
              case event of
                   KeyDown (Keysym key _ _) -> do
                       case key of
                            SDLK_q  -> return True
                            SDLK_s  -> saveBoard board
                            _       -> whileEvents
                   _   -> return False

-- create an initial game state from user input
userSeed :: Surface -> Board -> Cell -> IO Board
userSeed screen seed pos@(x, y) = do
    event   <- pollEvent
    case event of
         KeyDown (Keysym key _ _) -> do
             case key of
                  SDLK_UP       -> do
                      updateCursor (x, y - 1)
                  SDLK_DOWN     -> do
                      updateCursor (x, y + 1)
                  SDLK_LEFT     -> do
                      updateCursor (x - 1, y)
                  SDLK_RIGHT    -> do
                      updateCursor (x + 1, y)
                  SDLK_RETURN   -> do
                      drawBoard screen seed
                      addCell screen pos
                      SDL.flip screen
                      userSeed screen (pos : seed) pos
                  SDLK_b        -> return $ nub seed
                  _             -> userSeed screen seed pos
         _  -> userSeed screen seed pos
         where updateCursor (x, y) = do
                   drawBoard screen seed
                   drawCursor screen (x, y)
                   userSeed screen seed (x, y)

-- save the current game state to file as a plain-text Haskell list
saveBoard :: Board -> IO Bool
saveBoard b = do
    putStrLn "Where should the game state be saved to?"
    name    <- getLine
    writeFile name $ show b
    putStrLn $ "Finished saving to " ++ name ++ "."
    putStrLn "Continue playing?"
    cont    <- getLine
    case cont of
         "y"    -> return False
         _      -> return True

-- load a game state from a save file
loadBoard :: IO (Maybe Board)
loadBoard = do
    putStrLn "Load a game? (y/n)"
    yn  <- getLine
    case yn of
         "y"    -> do
             putStrLn "What is the name of the file?"
             contents   <- getLine >>= readFile
             return $ Just $ read contents
         _      -> return Nothing

main = withInit [InitEverything] $ do
    screen  <- setVideoMode width height depth [SWSurface]
    load    <- loadBoard
    let load' = case load of
                    Nothing -> []
                    Just xs -> xs
    drawBoard screen load'
    putStrLn "Use arrow keys to move cursor, enter to add a cell.\nPress 'b' to begin the game."
    seed    <- userSeed screen load' centre
    putStrLn "Press 'q' to quit.\nPress 's' to save."
    gen edges screen seed
    where intDiv n d = floor $ (fromIntegral n) / (fromIntegral d)
          centre     = (intDiv width (2 * cellsize), intDiv height (2 * cellsize))
          makeEdge   = Prelude.flip intDiv cellsize
          edges      = (makeEdge width, makeEdge height)
