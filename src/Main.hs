module Main where

import System.IO
import CandyCrushParser
import GameController

main :: IO ()
main = do
    putStrLn "Welcome to Candy Crush!"
    putStrLn "Please enter a configuration file name or choose one of [easy, medium, hard]: "
    putStrLn "(Default is 'config/hard', press Enter to use the default)"
    input <- candyGetLine
    let fileName = if null input
                   then "config/hard"  -- Default configuration
                   else case input of
                        "easy"   -> "config/easy"
                        "medium" -> "config/medium"
                        "hard"   -> "config/hard"
                        other    -> other
    result <- parseFile fileName
    case result of
        Left err -> do
            putStrLn "Error parsing the configuration file:"
            print err
            putStrLn "Please try again."
            main
        Right difficulty -> do
            putStrLn "Configuration loaded successfully!"
            gameLoop difficulty

