module Main (main) where

import Lib
import Data.Time.Clock
import System.IO

main :: IO ()
main = do
    randomStr <- randomString
    hSetBuffering stdout NoBuffering
    playGame randomStr

playGame :: String -> IO ()
playGame randomStr = do
  startTime <- getCurrentTime

  putStrLn randomStr
  putStrLn "Please enter a word: "
  putStr "> "
  attempt <- getLine
  
  endTime   <- getCurrentTime
  isSuccess <- checkAttempt attempt randomStr
  
  let 
    score     = if isSuccess then calculateScore startTime endTime attempt else 0
    congrats  = if isSuccess then "Congratulations! " else "Naah. "

  putStrLn $ congrats <> "Your score: " <> show score
  
  putStrLn "New Game? [Y/N]"
  putStr "> "
  answer <- getLine

  if answer == "Y" || answer == "y" then do
      newRandomStr <- randomString
      playGame newRandomStr
  else
      putStrLn "Bye!" 

