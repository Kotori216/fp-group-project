{-# LANGUAGE OverloadedStrings #-}

module Database (
    initialiseDB,
    getOrCreatePark,
    getOrCreateMovie,
    createRow,
    saveRows
) where

import Types
import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.Internal
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow

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

getOrCreatePark :: Connection -> String -> String -> String -> IO Park
getOrCreatePark conn name phone address = do
    results <- queryNamed conn "SELECT * FROM park WHERE name=:name" [":name" := name]    
    if length results > 0 then
        return . head $ results
    else do
        execute conn "INSERT INTO park (name, phone, address) VALUES (?, ?, ?)" (name, phone, address)
        getOrCreatePark conn name phone address

getOrCreateMovie :: Connection -> String -> Bool -> String -> Maybe String -> IO Movie
getOrCreateMovie conn title cc rating underwriter = do
    results <- queryNamed conn "SELECT * FROM movie WHERE title=:title" [":title" := title]    
    if length results > 0 then
        return . head $ results
    else do
        execute conn "INSERT INTO movie (title, cc, rating, underwriter) VALUES (?, ?, ?, ?)" (title, cc, rating, underwriter)
        getOrCreateMovie conn title cc rating underwriter

createRow :: Connection -> Row -> IO ()
createRow conn row = do
    park <- getOrCreatePark conn (park_ row) (phone_ row) (address_ row)
    movie <- getOrCreateMovie conn (movie_ row) (cc_ row) (rating_ row) (underwriter_ row)
    let event = Event {
        Types.id = id_ row,
        day = day_ row,
        date = date_ row,
        park = name park,
        movie = title movie
    }
    execute conn "INSERT INTO event VALUES (?,?,?,?,?)" event

saveRows :: Connection -> [Row] -> IO ()
saveRows conn = mapM_ (createRow conn)