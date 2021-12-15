{-# LANGUAGE OverloadedStrings #-}

module Parse (
    parseRows
) where

import Types
import Xeno.DOM
import Data.ByteString 
import qualified Data.ByteString.Lazy.Char8 as L8
import Control.Exception
import Data.Char (chr)

-- |The 'bsToStr' function converts a ByteString to a String.
bsToStr :: ByteString -> String
bsToStr = Prelude.map (chr . fromEnum) . unpack

-- |The 'getTextOfANode' function takes a Node and returns the text of the Node
getTextOfANode :: Node -> String
getTextOfANode node = bsToStr t where Text t = Prelude.head $ contents node

-- |The 'getTextOfChildren' function recursively finds the text of the child node which matches the child name
getTextOfChildren :: [Node] -> String -> Maybe String
getTextOfChildren [] _ = Nothing
getTextOfChildren (thisChild : nextChildren) childName 
    | bsToStr (Xeno.DOM.name thisChild) == childName = Just (getTextOfANode thisChild)
    | otherwise = getTextOfChildren nextChildren childName

-- |The 'parseRow' function converts a Node to a Row
parseRow :: Node -> Row
parseRow node =
    let rid = bsToStr $ snd $ Prelude.head $ attributes $ node
        Just rday = getTextOfChildren (children node) "day"
        Just rdate = getTextOfChildren (children node) "date"
        Just rpark = getTextOfChildren (children node) "park"
        Just rphone = getTextOfChildren (children node) "park_phone"
        Just rmovie = getTextOfChildren (children node) "title"
        rcc = (getTextOfChildren (children node) "cc") == Just "Y" 
        Just rrating = getTextOfChildren (children node) "rating"
        runderwriter = getTextOfChildren (children node) "underwriter"
        Just raddress = getTextOfChildren (children node) "park_address"
    in Row {
        id_ = rid,
        day_ = rday,
        date_ = rdate,
        park_ = rpark,
        movie_ = rmovie,
        cc_ = rcc,
        rating_ = rrating,
        underwriter_ = runderwriter,
        phone_ = rphone,
        address_ = raddress
    }

-- |The 'parseRows' function parses xml to Rows
parseRows :: L8.ByteString -> Rows
parseRows xml = 
    let rowList = Prelude.map parseRow $ xml2RowNodes xml
    in Rows {rows = rowList}

-- |The 'xml2RowNodes' function parses xml to Node list
xml2RowNodes :: L8.ByteString -> [Node]
xml2RowNodes xml = case parse (L8.toStrict xml) of
    Left err -> throw err
    Right result -> children $ Prelude.head $ children result 
