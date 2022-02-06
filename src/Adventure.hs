module Main where

import World
import Actions

import Control.Monad
import System.IO
import System.Exit

import System.Console.Haskeline

winmessage = "Congratulations, you have made it out of the house.\n" ++
             "Now go to your lectures..."

{- Given a game state, and user input (as a list of words) return a 
   new game state and a message for the user. -}

{-rather than taking in list of strings take list of commands-}

{-
parseInput :: [String] -> Maybe Command
parseInput ["go", d] = case parseDirection d of 
                            Nothing -> Nothing
                            Just dir -> Just dir
-}
process :: GameData -> [String] -> (GameData, String)
process state [cmd,arg] = case validate cmd arg of
                            Just com -> performAction state com
                            Nothing -> (state, "I don't understand")
process state [cmd]     = case instructions cmd of
                            Just fn -> fn state
                            Nothing -> (state, "I don't understand")
process state _ = (state, "I don't understand")

repl :: GameData -> InputT IO GameData
repl state | finished state = return state
repl state = do outputStrLn (show state)
                outputStrLn "What now? "
                --hFlush stdout
                cmd <- getInputLine "% "
                case cmd of
                   Just com -> do let (state', msg) = process state (words com)
                                  outputStrLn msg
                                  if (won state') then do outputStrLn winmessage
                                                          return state'
                                  else repl state'
                   Nothing -> do outputStrLn "Please enter a command"
                                 repl state
                
                

main :: IO ()
main = do runInputT defaultSettings (repl initState)
          return ()
--return ()