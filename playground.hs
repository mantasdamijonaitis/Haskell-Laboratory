import System.IO.Unsafe 
import Data.List.Split
import Data.List
import Data.Char(isSpace)

prepareListForComparison :: [Char] -> [[Char]]
prepareListForComparison oldList =
    map (dropWhile isSpace) (map sort (filter (not . null) (splitOn "\n" oldList)))
main = do
    let words = unsafePerformIO . readFile $ "data.txt"
    let splittedValues = splitOn "#" words
    let wordsToBeGuessed = splittedValues !! 0
    let guesses = splittedValues !! 1
    let processedWordsToBeGuessed = prepareListForComparison wordsToBeGuessed
    let processedGuesses = prepareListForComparison guesses
    let matches = map (\x -> map (\y -> length(intersect x y) >= length(y)) processedWordsToBeGuessed) processedGuesses
    let trueValuesAmount = map (\y -> length (filter (==True) y)) matches
    --print trueValuesAmount
    let resultAsStrings = concat . intersperse "\n" . map show $ trueValuesAmount
    --print resultAsStrings
    writeFile "results.txt" resultAsStrings