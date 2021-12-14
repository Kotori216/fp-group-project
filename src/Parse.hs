{-# LANGUAGE OverloadedStrings #-}

module Parse (
--    parseRows
) where

import Types
import Xeno.DOM
import Data.ByteString 
import qualified Data.ByteString.Lazy.Char8 as L8

--parseRows :: ByteString -> Either String Rows

let (Right result) = parse (L8.toStrict xml)

--xml2RowNodes :: ByteString -> [Node]
--xml2RowNodes xml = children $ Prelude.head $ children result
