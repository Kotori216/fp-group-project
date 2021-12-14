{-# LANGUAGE DeriveGeneric #-}

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
    title_ :: String,
    cc_ :: Bool,
    rating_ :: String,
    underwriter_ :: Maybe String,
    name_ :: String,
    phone_ :: String,
    address_ :: String
} deriving (Show, Generic)

data Rows = Rows {
    rows :: [Row]
} deriving (Show, Generic)