module Snake where

import           Control.Monad

import           System.Console.ANSI
import           System.IO
import           System.Timeout

inputTimeout = 250000         -- in microseconds

type Position = (Int, Int)

type GameState = (Command, Position)

initialGameState :: GameState
initialGameState = (MoveUp, (12, 40))

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

draw :: Position -> IO ()
draw (row, col) = do
  setCursorPosition row col
  putChar '@'
  setCursorPosition 25 0

clear :: Position -> IO ()
clear (row, col) = do
  setCursorPosition row col
  putChar ' '
  setCursorPosition 25 0

advance :: Command -> Position -> Position
advance MoveUp (row, col)    = (row - 1, col)
advance MoveDown (row, col)  = (row + 1, col)
advance MoveLeft (row, col)  = (row, col - 1)
advance MoveRight (row, col) = (row, col + 1)
advance _ state              = state


loop :: GameState -> IO ()
loop gameState = do
  let command = fst gameState
  let position = snd gameState
  let newPosition = advance command position

  clear position
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
