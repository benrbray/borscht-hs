{-# LANGUAGE QuasiQuotes #-}

module Main where


--------------------------------------------------------------------------------

import System.Exit (exitFailure)
import Data.Sequence as Seq ( fromList )

import Data.Aeson
import qualified Data.Aeson.Types as AT

import Text.RawString.QQ
import qualified Data.ByteString.Lazy.Char8 as BS

import Borscht.Req.Discogs.JSON

--------------------------------------------------------------------------------

main = do
    putStrLn "This test always fails!"
    exitFailure

searchResultJSON :: BS.ByteString
searchResultJSON = [r|{ "results" : [
    {
        "catno": "87",
        "uri": "/Mazouni-Ecoute-Moi-Camarade-Achhal-Dabart-Alik/master/1737699",
        "master_id": 1737699,
        "thumb": "https://img.discogs.com/TT7giSjuASwNwakqvUhhXzonvw0=/fit-in/150x150/filters:strip_icc():format(jpeg):mode_rgb():quality(40)/discogs-images/R-8288737-1458700348-8134.jpeg.jpg",
        "genre": ["Folk, World, & Country"],
        "format": ["Vinyl","7\"","45 RPM"],
        "community": {"have": 6,"want": 375},
        "title": "Mazouni* - Ecoute Moi Camarade / Achhal Dabart Alik",
        "cover_image": "https://img.discogs.com/2iWwflGEgPrn7MnsojaZ36M2_yI=/fit-in/600x600/filters:strip_icc():format(jpeg):mode_rgb():quality(90)/discogs-images/R-8288737-1458700348-8134.jpeg.jpg",
        "id": 1737699,
        "year": "1974",
        "style": [],
        "barcode": [],
        "user_data": {
            "in_collection": false,
            "in_wantlist": false
        },
        "label": ["DDA", "Cheniki"],
        "resource_url": "https://api.discogs.com/masters/1737699",
        "country": "Algeria",
        "master_url": "https://api.discogs.com/masters/1737699",
        "type": "master"
    }
] }
|]

runParseTest :: IO ()
runParseTest = do
    putStrLn "runParseTest\n\n"
    putStrLn "\n\n\n\ndecoding...\n\n"
    let x = eitherDecode searchResultJSON :: Either String DiscogsSearchResults
    print x
    print $ encode x
    BS.putStrLn $ encode (Seq.fromList [1,2,3::Int])