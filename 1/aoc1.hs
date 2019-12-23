import System.IO

readInput :: FilePath -> IO [String]
readInput path = do
    contents <- readFile path
    let res = lines contents
    return res

rInt :: String -> Int
rInt = read

convInput :: [String] -> [Int]
convInput list = map rInt list

-- Part1
fuel moduleMass = (div moduleMass 3 ) - 2

totalFuel fuelList = sum $  map fuel fuelList

-- Part 2
fuel2 moduleMass =
    if a > 0
        then a
        else 0
    where a = fuel moduleMass

fuelFuel moduleMass =
    if moduleMass <= 0
        then 0
        else fuel2 moduleMass + (fuelFuel (fuel2 moduleMass))

totalFuelFuel fuelList = sum $ map fuelFuel fuelList

main :: IO()
main = do
    contents <-  readFile "input.txt"
    let input = convInput $ lines contents
    let res = totalFuel input
    let res2 =  totalFuelFuel input
    print res
    print res2
