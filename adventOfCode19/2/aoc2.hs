import System.IO
import Data.List.Split

runProgram :: Int-> [Int] -> [Int]
runProgram codeNumber list =
    if step == [-1]
        then list
    else
        runProgram (codeNumber + 1) step
    where
        step = runIntCode (getIntCode codeNumber list) list

runIntCode :: [Int]-> [Int] -> [Int]
runIntCode intCode list =
    case code of
        1 -> replaceAtIndex dest ( list!!input1 + list !!input2 ) list
        2 -> replaceAtIndex dest ( list!!input1 * list !!input2 ) list
        99 -> [-1]
        where
                code = intCode!!0
                input1 = intCode!!1
                input2 = intCode!!2
                dest = intCode!!3

replaceAtIndex :: Int-> Int-> [Int] -> [Int]
replaceAtIndex index value (x:xs) =
    if index == 0
        then [value] ++ xs
    else
        [x] ++ replaceAtIndex (index -1) value xs

getIntCode :: Int -> [Int] -> [Int]
getIntCode n list =
    take 4 a where a = drop (n*4) list

-- Part2

setNounVerb :: Int -> Int -> [Int] -> [Int]
setNounVerb noun verb list =
    replaceAtIndex 1 noun (replaceAtIndex 2 verb list)

run :: [Int] -> [Int]
run list = runProgram 0 list

guessInputs :: Int -> [(Int, Int)] -> [Int] -> (Int,Int)
guessInputs output combs list =
    if runComb tuple list == output
        then tuple
    else
        guessInputs output (tail combs) list
        where tuple = head combs

runComb :: (Int, Int) -> [Int] -> Int
runComb tuple list =
    head result
        where result = run (setNounVerb (fst tuple) (snd tuple) list)

-- main :: IO
main = do
    contents <- readFile "input.txt"
    let input = map (read :: String -> Int ) (splitOn "," contents)
    let modifiedInput = replaceAtIndex 2 2 ( replaceAtIndex 1 12 input)
    print $ head (run modifiedInput)
    let res = guessInputs 19690720 [(x,y) | x <- [1..99], y <- [1..99]] input
    print res
