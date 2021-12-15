{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}


module Types (
    Event (..),
    Movie (..),
    Park (..),
    Row (..),
    Rows (..)
) where

import GHC.Generics

data Event = Event {
    id :: String,
    day :: String,
    date :: String,
    park :: String,
    movie :: String
} deriving (Show)

data Movie = Movie {
    title :: String,
    cc :: Bool,
    rating :: String,
    underwriter :: Maybe String
} deriving (Show)

data Park = Park {
    name :: String,
    phone :: String,
    address :: String
} deriving (Show)

data Row = Row {
    id_ :: String,
    day_ :: String,
    date_ :: String,
    park_ :: String,
    movie_ :: String,
    cc_ :: Bool,
    rating_ :: String,
    underwriter_ :: Maybe String,
    phone_ :: String,
    address_ :: String
} deriving (Generic)

instance Show Row where
    show a = "----On " ++ (day_ a) ++ ", " ++ (date_ a) ++ ", " ++ (movie_ a) 
        ++ " will be screened in " ++ (park_ a) ++ ", " ++ (address_ a) 
        ++ ". The rating of the movie is " ++ (rating_ a) ++ ". " ++ (showUnderwriter a) ++ (showCC a) 
        ++ " Please call " ++ (phone_ a) ++ " for more information."

showCC :: Row -> String
showCC a | cc_ a = "CC is provided." | otherwise = ""

showUnderwriter :: Row -> String
showUnderwriter a = case underwriter_ a of 
    Nothing -> ""
    Just writer -> "The underwriter is " ++ writer ++ "."

data Rows = Rows {
    rows :: [Row]
} deriving (Show, Generic)