import Data.List


data Letter = Large { letterID :: Int , status :: Status}
            | Medium { letterID :: Int, status :: Status}
            | Small { letterID :: Int, status :: Status} deriving (Show)

data Status = Delivered | Sent | Arrived | Rejected deriving (Show)

changeStatus :: Letter -> Status -> Letter
changeStatus letter new = letter {status = new}

large = Large 123 Sent
medium = Medium 124 Arrived
small = Small 125 Arrived



wordsInText :: [[Char]] -> [Char] -> Bool
wordsInText (word:rest) text = (word `isInfixOf` text) || (wordsInText rest text)
wordsInText _ _ = False

main :: IO ()
main =
    (print $ changeStatus large Arrived) >>
    (print $ changeStatus medium Rejected) >>
    (print $ changeStatus small Delivered)
