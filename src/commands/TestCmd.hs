
module TestCmd where

import Fuzzy (distanceCosine, normalized)
import Kakasi (fastSin, kakasiConfigure, kakasiConvert, kakasiClose)

import qualified Data.Text as Text 

------------------------------------------------------------

runTestCmd :: IO ()
runTestCmd = do
    -- convert string
    print $ normalized "東京に住んでいます"
    print $ distanceCosine 3 "mossissippi" "Mississippi"

testKakasi :: IO ()
testKakasi = do
    -- initialize kakasi
    status <- kakasiConfigure ["-i", "utf8", "-o", "utf8", "-Ja", "-Ha", "-Ka"]
    print status
    -- convert string
    -- (NOTE: currently does NOT function correctly)
    latin <- kakasiConvert "東京に住んでいます"
    -- close kakasi
    kakasiClose
    putStrLn (Text.unpack latin)

    

-- runTestCmd :: IO ()
-- runTestCmd = do
--     mapM_ (print . fastSin) [0/10, 1/10 .. 10/10]