module World where

import Data.Matrix -- provides a matrix datatype
import System.Random -- provides the Random typeclass
import Control.Monad.State.Lazy -- provides the State monad and MonadState typeclass

type Beepers = Int
type Grid = Matrix Beepers

type Pos = (Int, Int)
data Direction = North | South | East | West
    deriving (Eq, Ord, Bounded, Enum)

instance Show Direction where
    show North = "^"
    show West  = "<"
    show South = "v"
    show East  = ">"

instance Random Direction where
    random g = case randomR (fromEnum (minBound :: Direction),
                             fromEnum (maxBound :: Direction)) g of
                 (r, g') -> (toEnum r, g')
    randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
                        (r, g') -> (toEnum r, g')

data Label = Label { getState   :: (Maybe Direction)
                   , getBeepers :: Beepers }

instance Show Label where
    show l = harold ++ beepers
      where harold = case getState l of
                       Nothing -> " "
                       Just d  -> show d
            beepers = if getBeepers l > 0 then "x" else "o"

data World = World { getPos  :: Pos
                   , getDir  :: Direction
                   , getBag :: Int
                   , getGrid :: Grid }

instance Show World where
  show (World pos dir _ grid) = show $ orient $ labelHarold $ labelBeepers <$> grid
    where labelBeepers = Label Nothing
          labelHarold = do
              Label _ beeps <- uncurry getElem pos
              setElem (Label (Just dir) beeps) pos
          orient m = matrix (nrows m) (ncols m) $ \(i,j) -> m ! (nrows m - i + 1, j)

emptyGrid :: Int -> Int -> Grid
emptyGrid rows cols = matrix rows cols (const 0)

emptyWorld :: Int -> Int -> World
emptyWorld rows cols = World (1,1) East 0 $ emptyGrid rows cols

randomPos :: Int -> Int -> IO Pos
randomPos rows cols = do
    rowPos <- randomRIO (1, rows)
    colPos <- randomRIO (1, cols)
    return (rowPos, colPos)

randomEmptyWorld :: Int -> Int -> IO World
randomEmptyWorld rows cols = do
    pos <- randomPos rows cols
    dir <- randomIO
    pure $ World { getPos = pos
                 , getDir = dir
                 , getBag = 0 
                 , getGrid = emptyGrid rows cols }
