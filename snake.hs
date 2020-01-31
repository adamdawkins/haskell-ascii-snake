module Snake where

import           Control.Monad

import           System.Console.ANSI
import           System.IO
import           System.Timeout

inputTimeout = 500000         -- in microseconds

type Position = (Int, Int)

type GameState = (Command, [Position])

initialGameState :: GameState
initialGameState = (MoveUp, [(12, 40)])

data Command
  = MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight
  | Quit
  | Unknown

parseCommand :: Char -> Command
parseCommand 'k' = MoveUp
parseCommand 'j' = MoveDown
parseCommand 'h' = MoveLeft
parseCommand 'l' = MoveRight
parseCommand 'q' = Quit
parseCommand _   = Unknown

initScreen :: IO ()
initScreen = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  clearScreen

draw :: [Position] -> IO ()
draw ((row,col):body) = do
  setCursorPosition row col
  putChar '@'
  drawBody body
  where
    drawBody ((row, col):bs) = do
      setCursorPosition row col
      putChar 'o'
      drawBody bs
    drawBody [] = setCursorPosition 25 0

clear :: [Position] -> IO ()
clear ((row, col):ps) = do
  setCursorPosition row col
  putChar ' '
  clear ps

clear [] = setCursorPosition 25 0

advance :: Command -> [Position] -> [Position]
advance MoveUp    ((row, col):ps) = (row - 1, col) : (row,col) : ps
advance MoveDown  ((row, col):ps) = (row + 1, col) : (row,col) : ps
advance MoveLeft  ((row, col):ps) = (row, col - 1) : (row,col) : ps
advance MoveRight ((row, col):ps) = (row, col + 1) : (row,col) : ps
advance _ state                   = state


loop :: GameState -> IO ()
loop gameState = do
  let command = fst gameState
  let positions = snd gameState
  let newPosition = advance command positions

  clear positions
  draw newPosition

  c <- timeout inputTimeout getChar -- wait for input, with timeout

  case c of
    -- no input given
    Nothing    -> loop (command, newPosition)

    -- quit on 'q'
    Just 'q'   -> putStrLn "quitting..."

    -- input given
    Just input -> loop (newCommand, advance command (snd gameState))
      where newCommand = parseCommand input

main :: IO ()
main = do
  initScreen
  loop initialGameState
