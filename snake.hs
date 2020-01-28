module Snake where

import Control.Monad

import System.Console.ANSI
import System.IO

type Position = (Int, Int)

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
parseCommand _ = Unknown

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
advance MoveUp (row, col) = (row - 1, col)
advance MoveDown (row, col) = (row + 1, col)
advance MoveLeft (row, col) = (row, col - 1)
advance MoveRight (row, col) = (row, col + 1)
advance _ state = state

-- playGame :: Position -> Command -> IO ()
playGame currentState command = do
  let newState = advance command currentState
  clear currentState
  draw newState
  return newState

main :: IO ()
main = do
  initScreen
  draw (12, 40)
  userInput <- getContents
  let commands = map parseCommand userInput
  foldM_ playGame (12, 40) commands
