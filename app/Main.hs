module Main where

import Fetch
import Parse
import Xeno.DOM
import Data.ByteString 
import qualified Data.ByteString.Lazy.Char8 as L8

main :: IO ()
main = do
    let url = "https://data.cityofchicago.org/api/views/7piw-z6r6/rows.xml"
    print "Downloading..."
    xml <- download url
    print "=========Node"
    let node = Prelude.head $ xml2RowNodes xml
    print node
    print "=========name"
    print $ name node
    print "=========attributes"
    print $ attributes node
    print "=========contents"
    print $ contents node
    print "=========children"
    print $ children node
    print "=========first child contents text"
    let Text t = Prelude.head $ contents $ Prelude.head $ children node
    print t
    print "=========first movie"
    print $ parseMovie node

