{-# LANGUAGE OverloadedStrings #-}

module Parse (
--    parseRows
    xml2RowNodes,
    parseMovie
) where

import Types
import Xeno.DOM
import Data.ByteString 
import qualified Data.ByteString.Lazy.Char8 as L8
import Control.Exception
import Data.Char (chr)

bsToStr :: ByteString -> String
bsToStr = Prelude.map (chr . fromEnum) . unpack

getTextOfANode :: Node -> String
getTextOfANode node = bsToStr t where Text t = Prelude.head $ contents node

getTextOfChildren :: [Node] -> String -> Maybe String
getTextOfChildren [] _ = Nothing
getTextOfChildren (thisChild : nextChildren) childName 
    | bsToStr (Xeno.DOM.name thisChild) == childName = Just (getTextOfANode thisChild)
    | otherwise = getTextOfChildren nextChildren childName

parseMovie :: Node -> Movie
parseMovie node =
    let Just mtitle = getTextOfChildren (children node) "title"
        mcc = (getTextOfChildren (children node) "cc") == Just "Y" 
        Just mrating = getTextOfChildren (children node) "rating"
        munderwriter = getTextOfChildren (children node) "underwriter"
    in Movie {title = mtitle, cc = mcc, rating = mrating, underwriter = munderwriter}

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
 

xml2RowNodes :: L8.ByteString -> [Node]
xml2RowNodes xml = case parse (L8.toStrict xml) of
    Left err -> throw err
    Right result -> children $ Prelude.head $ children result 
