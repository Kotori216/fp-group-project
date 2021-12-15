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

getTextOfChildren :: [Node] -> String -> String
getTextOfChildren (thisChild : nextChildren) childName 
    | bsToStr (Xeno.DOM.name thisChild) == childName = getTextOfANode thisChild
    | otherwise = getTextOfChildren nextChildren childName

parseMovie :: Node -> Movie
parseMovie node =
    let mtitle = getTextOfChildren (children node) "title"
        mcc = (getTextOfChildren (children node) "cc") == "Y" 
        mrating = getTextOfChildren (children node) "rating"
    in Movie {title = mtitle, cc = mcc, rating = mrating, underwriter = Nothing}
 

xml2RowNodes :: L8.ByteString -> [Node]
xml2RowNodes xml = case parse (L8.toStrict xml) of
    Left err -> throw err
    Right result -> children $ Prelude.head $ children result 
