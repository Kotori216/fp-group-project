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
    parkAllEvents <- queryParkAllEvents conn
    mapM_ print parkAllEvents
    movieAllEvents <- queryMovieAllEvents conn
    mapM_ print movieAllEvents
    parkMovieAllEvents <- queryParkMovieAllEvents conn
    mapM_ print parkMovieAllEvents


