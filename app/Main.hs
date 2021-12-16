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
    Prelude.putStrLn "----------------------------------------------------"
    Prelude.putStrLn "  Welcome to the Movie info app                     "
    Prelude.putStrLn "  Please choose a query option below:               "
    Prelude.putStrLn "  (1) Download Data                                 "    
    Prelude.putStrLn "  (2) Query by entering the park name               "
    Prelude.putStrLn "  (3) Query by entering the movie name              "
    Prelude.putStrLn "  (4) Query by entering both the park & movie name  "
    Prelude.putStrLn "  (5) Quit                                          "
    Prelude.putStrLn "----------------------------------------------------"
    conn <- initialiseDB
    option <- readLn :: IO Int
    case option of
        1 -> do
            let url = "https://data.cityofchicago.org/api/views/7piw-z6r6/rows.xml"
            Prelude.putStrLn "Downloading..."
            xml <- download url
            Prelude.putStrLn "Parsing..."
            let rows_ = rows $ parseRows xml
            Prelude.putStrLn "Saving on DB..."
            saveRows conn rows_
            Prelude.putStrLn "Saved!"
            Prelude.putStrLn "The XML is now ready for query processing" 
            main
        2 -> do
            parkAllEvents <- queryParkAllEvents conn
            case parkAllEvents of
                [] -> Prelude.putStrLn "Could't find events for the given park name"
                otherwise -> mapM_ print parkAllEvents
            main
        3 -> do
            movieAllEvents <- queryMovieAllEvents conn
            case movieAllEvents of
                [] -> Prelude.putStrLn "Could't find events for the given movie name"
                otherwise -> mapM_ print movieAllEvents
            main
        4 -> do
            parkMovieAllEvents <- queryParkMovieAllEvents conn
            case parkMovieAllEvents of
                [] -> Prelude.putStrLn "Could't find events for the given park & movie name"
                otherwise -> mapM_ print parkMovieAllEvents
            main
        5 -> Prelude.putStrLn "Hope you've enjoyed using the app!"
        otherwise -> Prelude.putStrLn "Invalid option"
