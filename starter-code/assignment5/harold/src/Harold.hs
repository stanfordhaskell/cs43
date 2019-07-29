module Harold where

import Data.Matrix
import Control.Monad.State.Lazy
import Control.Monad.Extra

import World

move :: State World ()
move = do
    dir   <- gets getDir
    (r,c) <- gets getPos
    let newPos = case dir of
                   North -> (r + 1, c)
                   South -> (r - 1, c)
                   East  -> (r, c + 1)
                   West  -> (r, c - 1)
    whenM frontIsClear $ modify (\w -> w { getPos = newPos })

turnLeft :: State World ()
turnLeft = do
    dir <- gets getDir
    let newDir = case dir of
                   North -> West
                   West  -> South
                   South -> East
                   East  -> North
    modify (\w -> w { getDir = newDir })

putBeeper :: State World ()
putBeeper = do
    pos <- gets getPos
    grid <- gets getGrid
    bagBeeps <- gets getBag
    let cellBeeps = grid ! pos
    whenM beepersInBag $ modify (\w ->
        w { getGrid = setElem (cellBeeps + 1) pos grid 
          , getBag = bagBeeps })

frontIsClear :: State World Bool
frontIsClear = do
    World (r,c) dir _ grid <- get
    pure $ case dir of
             North -> r < nrows grid
             West  -> c > 1
             South -> r > 1
             East  -> c < ncols grid

beepersPresent :: State World Bool
beepersPresent = do
    pos <- gets getPos
    gets ((> 0) . (! pos) . getGrid)

-- INCOMPLETE return whether any beepers are in the bag
beepersInBag :: State World Bool
beepersInBag = pure False

facing :: State World Direction
facing = gets getDir

-- INCOMPLETE increment the number of beepers in the bag, return ()
craftBeeper :: State World ()
craftBeeper = pure ()
