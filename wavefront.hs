module Main where

import           Data.List           (foldl')
import           Data.List.Split     (splitOn)
import           Data.Matrix
import           Data.Maybe          (isJust, isNothing)
import           System.Console.ANSI
import           Text.Read           (readMaybe)

data Square = Block | Sqa Int deriving (Eq)
type Grid = Matrix Square

instance Show Square where
  show Block   = "▓"
  show (Sqa x) = show x

-- generate a grid with specified obstacles.
makeGrid :: Int -> Int -> [(Int, Int)] -> Grid
makeGrid len width obs = matrix (len + 2) (width + 2) $ \(i, j) ->
  if (i == 1 || i == (len + 2) || j == 1 || j == (width + 2))
  then Block
  else if (i, j) `elem` obs
    then Block
    else Sqa 0


-- von Neumann neighborhood.
-- generates the coordinates of the cells in the neighborhood, based on
-- the coordinates of central cell.
vonNeumannHood :: (Int, Int) -> [(Int, Int)]
vonNeumannHood (x, y) = [ (x - 1, y)   -- left
                        , (x, y - 1)   -- up
                        , (x + 1, y)   -- right
                        , (x, y + 1)]  -- down

-- Moore neighborhood.
mooreNeighborhood :: (Int, Int) -> [(Int, Int)]
mooreNeighborhood (x, y) = [ (x - 1, y)      -- left
                           , (x - 1, y - 1)  -- upper left
                           , (x, y - 1)      -- up
                           , (x + 1, y - 1)  -- upper right
                           , (x + 1, y)      -- right
                           , (x + 1, y + 1)  -- lower right
                           , (x, y + 1)      -- down
                           , (x - 1, y + 1)] -- lower left

{-
compute the number of steps between a given starting position and all the
otherblocks in the grid, until a certain block (the goal) is found.
there are two conditions for termination.
1.- there are no more paths to go to. The wavefront variable is empty.
    the grid is returned, together with a false value.
2.- the goal block exists in the current wavefront.
    the grid is returned as is, with a true value.
-}
wavefront' :: [(Int, Int)] -- the wavefront
           -> Int          -- current number of steps
           -> ((Int, Int) -> [(Int, Int)])  -- the neighborhood function
           -> Grid         -- the current grid
           -> (Grid, Bool) -- return the next iteration of the grid
wavefront' front steps hood grid
  | elem (Sqa (-1)) $ (flip (uncurry getElem)) grid <$> front = (newGrid, True)
  | null front = (grid, False)
  | otherwise = wavefront' newFront (steps + 1) hood newGrid
  where
    -- takes all the neighbors of the front cells, filters the ones with
    -- zeros on them, and modifies the matrix with their new values. The
    -- return value is a tuple with the new matrix and the new front.
    newFront = filter (\a -> (uncurry getElem) a grid == Sqa 0)
                      (concat $ hood <$> front)
    newGrid = foldl' (flip $ setElem $ Sqa (steps + 1)) grid newFront

-- generate shortest path in between two specified points on a grid.
wavefront :: (Int, Int)
          -> (Int, Int)
          -> ((Int, Int) -> [(Int, Int)])
          -> Grid
          -> (Grid, Bool)
wavefront init end hood grid
  = wavefront' [init] 1 hood (setElem (Sqa (-1)) end
                              (setElem (Sqa (1)) init grid))

getNumber :: String -> IO Int
getNumber msg = do
  putStrLn msg
  numString <- getLine
  if isJust (readMaybe numString :: Maybe Int)
  then return $ read numString
  else do
    putStrLn "Número inválido. Por favor ingrese de nuevo"
    getNumber msg

getCoord :: String -> IO (Int, Int)
getCoord msg = do
  putStrLn msg
  putStrLn $ "Por favor ingrese la coordenada X y luego la Y, separadas" ++
    " por un espacio:"
  coordsString <- getLine
  if (null $ filter isNothing
      $ (readMaybe <$> splitOn " " coordsString :: [Maybe Int]))
  then do
    let [x, y] = take 2 $ read <$> splitOn " " coordsString
    return $ (x, y)
  else do
    putStrLn "Los números dados no son válidos. Por favor, ingréselos de nuevo"
    getCoord msg

getValueList :: IO a -> String -> [a] -> IO [a]
getValueList ioac msg acc = do
  putStrLn msg
  ans <- getLine
  if (ans == "N")
  then return acc
  else do
    item <- ioac
    getValueList ioac msg (item : acc)

main :: IO ()
main = do
  putStrLn "Bienvenido al generador de caminos."
  numRows <- getNumber "Por favor ingrese el número de filas en el mapa."
  numCols <- getNumber "Ahora ingrese el número de columnas."
  obstacles <- getValueList (getCoord "ingresa la coordenada del obstáculo.")
    "para ingresar otra coordenada, presione Y. De lo contrario, presione N." []
  putStrLn "Su mapa está listo:"
  let mapa = makeGrid numRows numCols obstacles
  print mapa

  init <- getCoord "Ingrese el punto de partida:"
  end <- getCoord "Ingrese el punto de llegada:"
  putStrLn $ "Finalmente, diga si quiere considerar las casillas diagonales" ++
    " como vecinas. (Y/N)"
  hoodString <- getLine

  -- clearScreen
  putStrLn "Su camino se está calculando."
  let hood = if hoodString == "Y" then mooreNeighborhood else vonNeumannHood
  let (finalGrid, result) = wavefront init end hood mapa
  if result
  then do
    putStrLn "El mapa final es este:"
    print finalGrid
  else do
    putStrLn "Parece no haber un camino. :("
    print finalGrid
