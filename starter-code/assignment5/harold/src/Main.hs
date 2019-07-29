module Main where

import Data.Matrix

import Control.Monad.State.Lazy
import Control.Monad.Loops

import System.Random

import Harold
import World

main :: IO ()
main = do
    world <- randomEmptyWorld 5 6
    putStrLn $ show world
    putStrLn $ show $ execState program world

program :: State World ()
program = do
    markCorners

markCorners :: State World ()
markCorners = do
    moveToStart
    replicateM_ 4 craftBeeper
    replicateM_ 4 (moveToWall *> putBeeper *> turnLeft)

moveToStart :: State World ()
moveToStart = do
    face South
    moveToWall
    face West
    moveToWall
    face East

face :: Direction -> State World ()
face dir = whileM_ ((/= dir) <$> facing) turnLeft -- whileM_ is from Control.Monad.Loops

-- INCOMPLETE return () and move forward till Harold hits a wall
moveToWall :: State World ()
moveToWall = return ()
