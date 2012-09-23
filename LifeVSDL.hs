import LifeV
import Control.Monad
import Graphics.UI.SDL as SDL
import Data.List (nub)
import qualified Data.Vector as V

cellsize = 4

cellToRect :: Cell -> Rect
cellToRect (x, y) = Rect (cellsize * x) (cellsize * y) cellsize cellsize

addCell :: Surface -> Cell -> IO Bool
addCell screen cell = do
    pixel   <- mapRGB (surfaceGetPixelFormat screen) 255 255 255
    fillRect screen (Just $ cellToRect cell) pixel

drawBoard :: Surface -> Board -> IO ()
drawBoard screen b = do
    pixel   <- mapRGB (surfaceGetPixelFormat screen) 0 0 0
    fillRect screen Nothing pixel
    V.mapM_ (addCell screen) b
    SDL.flip screen

drawCursor :: Surface -> Cell -> IO ()
drawCursor screen cell = do
    pixel   <- mapRGB (surfaceGetPixelFormat screen) 125 125 125
    fillRect screen (Just $ cellToRect cell) pixel
    SDL.flip screen

gen :: Edge -> Surface -> Board -> IO ()
gen edge screen board = do
    quit    <- whileEvents
    drawBoard screen board
    unless quit (gen edge screen $ life board)
    where whileEvents = do
              event     <- pollEvent
              case event of
                   KeyDown (Keysym key _ _) -> do
                       case key of
                            SDLK_q  -> return True
                            SDLK_s  -> saveBoard board
                            _       -> whileEvents
                   _   -> return False

userSeed :: Surface -> [Cell] -> Cell -> IO Board
userSeed screen seed pos@(x, y) = do
    event   <- pollEvent
    case event of
         KeyDown (Keysym key _ _) -> do
             case key of
                  SDLK_UP       -> do
                      drawCursor screen (x, y - 1)
                      userSeed screen seed (x, y - 1)
                  SDLK_DOWN     -> do
                      drawCursor screen (x, y + 1)
                      userSeed screen seed (x, y + 1)
                  SDLK_LEFT     -> do
                      drawCursor screen (x - 1, y)
                      userSeed screen seed (x - 1, y)
                  SDLK_RIGHT    -> do
                      drawCursor screen (x + 1, y)
                      userSeed screen seed (x + 1, y)
                  SDLK_RETURN   -> do
                      addCell screen pos
                      SDL.flip screen
                      userSeed screen (pos : seed) pos
                  SDLK_d        -> return $ V.fromList $ nub seed
                  _             -> userSeed screen seed pos
         _  -> userSeed screen seed pos

saveBoard :: Board -> IO Bool
saveBoard b = do
    putStrLn "Where should the game state be saved to?"
    name    <- getLine
    writeFile name $ show b
    putStrLn $ "Finished saving to " ++ name
    putStrLn "Continue playing?"
    cont    <- getLine
    case cont of
         "y"    -> return False
         _      -> return True

loadBoard :: IO (Maybe Board)
loadBoard = do
    putStrLn "Load a game?"
    yn  <- getLine
    case yn of
         "y"    -> do
             putStrLn "what is the name of the file?"
             contents   <- getLine >>= readFile
             return $ Just $ read contents
         _      -> return Nothing

main = withInit [InitEverything] $ do
    screen  <- setVideoMode width height depth [SWSurface]
    {-seed    <- userSeed screen [] origin-}
    gen edges screen $ V.map (translate origin) fPent
    where makeEdge x = floor $ (fromIntegral x) / (fromIntegral cellsize)
          edges = (makeEdge width, makeEdge height)
          translate (l, u) (x, y) = (x + l, y + u)
          height = 480
          width  = 640
          depth  = 32
          origin = (60, 50)
