import System.IO

binPacking binSize itemArray = binPacking' [] binSize itemArray

binPacking' packedBins binSize [] = packedBins
binPacking' _         binSize (x:xs)
    | x > binSize = error "x greater than binSize"
binPacking' []         binSize (x:xs) = binPacking' [[x]] binSize xs
binPacking' (topBin:binRest) binSize (x:xs)
    | sum(topBin) + x <= binSize = binPacking' ((x:topBin):binRest) binSize xs
    | otherwise = topBin:(binPacking' binRest binSize (x:xs))

runPack = do
    contents <- readFile "input.txt"
    let items = map (\x -> read x ::Int) (lines contents)
        output = binPacking 100 items
        outputAsStrings = map (\x -> show x) output
    mapM_ putStrLn outputAsStrings
    
