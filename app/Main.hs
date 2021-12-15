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
    print "Downloading..."
    xml <- download url
    print "Parsing..."
    let rows_ = rows $ parseRows xml
    conn <- initialiseDB
    print "Saving on DB..."
    saveRows conn rows_
    print "Saved!"
    print "The XML is now ready for query processing"
    print "----------------------------------------------------"
    print "  Welcome to the Movie info app                     "
    print "  Plsease choose an query option below:             "
    print "  (1) Query by entering the park name               "
    print "  (2) Query by entering the movie name              "
    print "  (3) Query by entering both the park & movie name  "
    print "  (4) Quit                                          "
    print "----------------------------------------------------"
    option <- readLn :: IO Int
    case option of
        1 -> do
            parkAllEvents <- queryParkAllEvents conn
            case parkAllEvents of
                Nothing -> print "Could't find events for the given park name"
            mapM_ print parkAllEvents
            main
        2 -> do
            movieAllEvents <- queryMovieAllEvents conn
            case movieAllEvents of
                Nothing -> print "Could't find events for the given movie name"
            mapM_ print movieAllEvents
            main
        3 -> do
            parkMovieAllEvents <- queryParkMovieAllEvents conn
            case parkMovieAllEvents of
                Nothing -> print "Could't find events for the given park & movie name"
            mapM_ print parkMovieAllEvents
            main
        4 -> print "Hope you've enjoyed using the app!"
        otherwise -> print "Invalid option"
