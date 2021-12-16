{-# LANGUAGE OverloadedStrings #-}
-- {-# OPTIONS_HADDOCK show-extensions #-}

module Database (
    initialiseDB,
    getOrCreatePark,
    getOrCreateMovie,
    createRow,
    saveRows,
    queryParkAllEvents,
    queryMovieAllEvents,
    queryParkMovieAllEvents
) where

import Types
import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.Internal
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow

-- | create tables
initialiseDB :: IO Connection
initialiseDB = do
        conn <- open "row.sqlite"
        execute_ conn "CREATE TABLE IF NOT EXISTS `event` (\
            \`id` VARCHAR(100) NOT NULL,\
            \`day` VARCHAR(45) NULL,\
            \`date` VARCHAR(45) NULL,\
            \`park` VARCHAR(45) NULL,\
            \`movie` VARCHAR(45) NULL,\
            \PRIMARY KEY (`id`))"
        execute_ conn "CREATE TABLE IF NOT EXISTS `movie` (\
            \`title` VARCHAR(120) NOT NULL,\
            \`cc` BLOB NULL,\
            \`rating` VARCHAR(45) NULL,\
            \`underwriter` VARCHAR(45) NULL,\
            \PRIMARY KEY (`title`))"
        execute_ conn "CREATE TABLE IF NOT EXISTS `park` (\
            \`name` VARCHAR(120) NOT NULL,\
            \`phone` VARCHAR(45) NULL,\
            \`address` VARCHAR(120) NULL,\
            \PRIMARY KEY (`name`))"
        return conn


instance FromRow Row where
    fromRow = Row <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field


instance FromRow Park where
    fromRow = Park <$> field <*> field <*> field

instance ToRow Park where
    toRow (Park name phone address)
        = toRow (name, phone, address)

instance FromRow Movie where
    fromRow = Movie <$> field <*> field <*> field <*> field

instance ToRow Movie where
    toRow (Movie title cc rating underwriter)
        = toRow (title, cc, rating, underwriter)

instance FromRow Event where
    fromRow = Event <$> field <*> field <*> field <*> field <*> field

instance ToRow Event where
    toRow (Event id day date park movie)
        = toRow (id, day, date, park, movie)

-- | if this park not in the table, insert it
getOrCreatePark :: Connection -> String -> String -> String -> IO Park
getOrCreatePark conn name phone address = do
    results <- queryNamed conn "SELECT * FROM park WHERE name=:name" [":name" := name]    
    if length results > 0 then
        return . head $ results
    else do
        execute conn "INSERT INTO park (name, phone, address) VALUES (?, ?, ?)" (name, phone, address)
        getOrCreatePark conn name phone address

-- | if this movie not in the table, insert it
getOrCreateMovie :: Connection -> String -> Bool -> String -> Maybe String -> IO Movie
getOrCreateMovie conn title cc rating underwriter = do
    results <- queryNamed conn "SELECT * FROM movie WHERE title=:title" [":title" := title]    
    if length results > 0 then
        return . head $ results
    else do
        execute conn "INSERT INTO movie (title, cc, rating, underwriter) VALUES (?, ?, ?, ?)" (title, cc, rating, underwriter)
        getOrCreateMovie conn title cc rating underwriter


-- | insert a row into the table.
createRow :: Connection -> Row -> IO Event
createRow conn row = do
    let query_id = id_ row
    results <- queryNamed conn "SELECT * FROM event WHERE id=:id" [":id" := query_id]
    park <- getOrCreatePark conn (park_ row) (phone_ row) (address_ row)
    movie <- getOrCreateMovie conn (movie_ row) (cc_ row) (rating_ row) (underwriter_ row)
    let event = Event {
        Types.id = id_ row,
        day = day_ row,
        date = date_ row,
        park = name park,
        movie = title movie
    }

    if length results > 0 then
        return . head $ results
    else do
        execute conn "INSERT INTO event VALUES (?,?,?,?,?)" event
        createRow conn row


-- | insert rows into the table
saveRows :: Connection -> [Row] -> IO ()
saveRows conn = mapM_ (createRow conn)

-- | query all events that happens on the input park
queryParkAllEvents :: Connection -> IO [Row]
queryParkAllEvents conn = do
    putStrLn "Enter park name > "
    parkName <- getLine
    putStrLn $ "Looking for " ++ parkName ++ " events..."
    let sql = "SELECT id, day, date, park, movie, cc, rating, underwriter, phone, address FROM event inner join park on event.park == park.name inner join movie on event.movie == movie.title WHERE event.park=?"
    query conn sql [parkName]

-- | query all events of the input movie
queryMovieAllEvents :: Connection -> IO [Row]
queryMovieAllEvents conn = do
    putStrLn "Enter movie name > "
    movieName <- getLine
    putStrLn $ "Looking for " ++ movieName ++ " events..."
    let sql = "SELECT id, day, date, park, movie, cc, rating, underwriter, phone, address FROM event inner join park on event.park == park.name inner join movie on event.movie == movie.title WHERE event.movie=?"
    query conn sql [movieName]

-- | query all events of the input movie and happens in the input park
queryParkMovieAllEvents :: Connection -> IO [Row]
queryParkMovieAllEvents conn = do
    putStrLn "Enter park name > "
    parkName <- getLine
    putStrLn "Enter movie name > "
    movieName <- getLine
    putStrLn $ "Looking for " ++ parkName ++ " and " ++ movieName ++ " events..."
    let sql = "SELECT id, day, date, park, movie, cc, rating, underwriter, phone, address FROM event inner join park on event.park == park.name inner join movie on event.movie == movie.title WHERE event.park=? and event.movie=?"
    query conn sql [parkName, movieName]

-- queryCountryTotalCases :: Connection -> IO ()
-- queryCountryTotalCases conn = do
--     countryEntries <- queryCountryAllEntries conn
--     let total = sum (map cases countryEntries)
--     print $ "Total entries: " ++ show(total)
