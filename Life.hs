-- Implementation of Conway's Game of Life in the command line

import System.Random
import Control.Concurrent
import System.Environment

data Game = Game [(Int, [(Int, CellState)])]
    deriving(Show)

data CellState 
    = Alive
    | Dead
    deriving(Show, Eq)


main :: IO ()
main = do
    -- Pass width, height, and framerate into the program as command line arguments
    args <- getArgs
    let width = readArg 0 args
    let height = readArg 1 args - 1
    let fps = readArg 2 args
    print fps

    -- Initialize and run the game
    game <- initGame height width
    runGame fps game
  where readArg i ls = (read (ls !! i))


-- Game loop
runGame :: Int -> Game -> IO ()
runGame fps game = do
    printGame game
    threadDelay (1000000 `div` fps)
    runGame fps (updateGame game)



{---------------------------------------------}
{------------ GAME INITIALIZATION ------------}
{---------------------------------------------}

-- Initialize game with certain width and height
initGame :: Int -> Int -> IO Game
initGame width height = do
    r <- randomCells width height []
    return $ Game $ indexList $ map indexList r

-- return a list with indices 0-n, e.g. ['a', 'b', 'c'] -> [(0, 'a'), (1, 'b'), (2, 'c')]
indexList :: [a] -> [(Int, a)]
indexList ls = zip [0..] ls


-- Generate a random list of cell states of length n
randomList :: Int -> [CellState] -> IO [CellState]
randomList n ls 
  | n == 0 = return ls
  | otherwise = do
      r <- randomIO :: IO Bool
      let s = if r then Alive else Dead
      randomList (n - 1) (s : ls)


-- Generate a random list of lists of cell states of dimensions n by m
randomCells :: Int -> Int -> [[CellState]] -> IO [[CellState]]
randomCells n m ls 
  | n == 0 = return ls
  | otherwise = do
      r <- randomList m []
      randomCells (n - 1) m (r : ls)



{--------------------------------------------}
{--------------- GAME UPDATE ----------------}
{--------------------------------------------}

-- Determine which cells need to go from Dead to Alive and Alive to Dead
updateGame :: Game -> Game
updateGame game@(Game g) = 
    Game $ 
        map (\(i, l) -> 
            (i, map (\(j, cell) -> 
                (j, updateCell game i j cell))
                -- (j, Alive))
            l)
        ) g


updateCell :: Game -> Int -> Int -> CellState -> CellState
updateCell g i j cellState = 
    let livingNeighbors = countLivingNeighbors g i j
     in case cellState of 
          Alive 
            | livingNeighbors < 2 || livingNeighbors > 3 -> Dead 
            | otherwise -> Alive
          Dead
            | livingNeighbors == 3 -> Alive 
            | otherwise -> Dead


countLivingNeighbors :: Game -> Int -> Int -> Int
countLivingNeighbors (Game g) i j = 
    let alive  = filter (== Alive) 
                    [getEltSafe g n m | n <- [i-1..i+1],
                                        m <- [j-1..j+1],
                                        not (n == i && m == j)]
     in length alive
  where imax = length g
        jmax = length (snd (g !! 0))
        boundsCheck x y =  (x >= 0 && y >= 0 && x < imax && y < jmax)
        getElt game x y = snd (snd (game !! x) !! y)
        getEltSafe game x y = if boundsCheck x y then getElt game x y else Dead


{---------------------------------------------}
{--------------- GAME PRINTER ----------------}
{---------------------------------------------}

cellToChar :: CellState -> Char
cellToChar s = 
    case s of 
      Alive -> 'x'
      Dead  -> ' '


-- Return game in a printable format, stripping out indices
gameToString :: Game -> String
gameToString (Game g) = unlines $ map (map (cellToChar . snd) . snd) g


printGame :: Game -> IO ()
printGame = putStr . gameToString
