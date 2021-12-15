module Main where

import Fetch
import Parse
import Database
import Types
import Xeno.DOM
import Data.ByteString 
import qualified Data.ByteString.Lazy.Char8 as L8

main :: IO ()
main = do
    let url = "https://data.cityofchicago.org/api/views/7piw-z6r6/rows.xml"
    putStrLn "Downloading..."
    xml <- download url
    putStrLn "Parsing..."
    let rows_ = rows $ parseRows xml
    conn <- initialiseDB
    putStrLn "Saving on DB..."
    saveRows conn rows_
    putStrLn "Saved!"
    putStrLn "The XML is now ready for query processing"
    putStrLn "----------------------------------------------------"
    putStrLn "  Welcome to the Movie info app                     "
    putStrLn "  Plsease choose an query option below:             "
    putStrLn "  (1) Query by entering the park name               "
    putStrLn "  (2) Query by entering the movie name              "
    putStrLn "  (3) Query by entering both the park & movie name  "
    putStrLn "  (4) Quit                                          "
    putStrLn "----------------------------------------------------"
    option <- readLn :: IO Int
    case option of
        1 -> do
            parkAllEvents <- queryParkAllEvents conn
            case parkAllEvents of
                [] -> putStrLn "Could't find events for the given park name"
                otherwise -> mapM_ print parkAllEvents
            main
        2 -> do
            movieAllEvents <- queryMovieAllEvents conn
            case movieAllEvents of
                [] -> putStrLn "Could't find events for the given movie name"
                otherwise -> mapM_ print movieAllEvents
            main
        3 -> do
            parkMovieAllEvents <- queryParkMovieAllEvents conn
            case parkMovieAllEvents of
                [] -> putStrLn "Could't find events for the given park & movie name"
                otherwise -> mapM_ print parkMovieAllEvents
            main
        4 -> putStrLn "Hope you've enjoyed using the app!"
        otherwise -> putStrLn "Invalid option"
