// For more information see https://aka.ms/fsharp-console-apps

open System
open System.IO

let input = File.ReadAllText "./input.txt"

let levels = input.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)

let isWithinSafeParameters (level: int array) =
    let isIncreasing = level[0] < level[1]
    
    let differences = level |> Array.pairwise |> Array.map(fun (x, y) -> if isIncreasing then y - x else x - y)
    let unsafe = differences |> Array.filter(fun x -> x < 1 || x > 3)
    
    unsafe.Length = 0

let isSafe (level: string) =
    let values = level.Split ' ' |> Array.map(int)
    let isSafe = isWithinSafeParameters values
    isSafe
let safeLevels = levels |> Array.filter(isSafe)

printf $"The number of safe levels is %i{safeLevels.Length}"

let applyDapener (level: int array) =
    seq {
        for i in [0..(level.Length - 1)] do
            yield level |> Array.removeAt i
    } |> Seq.toArray

let isSafeWithDapener(level: string) =
    let values = level.Split ' ' |> Array.map(int) |> applyDapener
    let safeValues = values |> Array.filter(isWithinSafeParameters)
    safeValues.Length > 0


let dapenedSafeLevels = levels |> Array.filter(isSafeWithDapener)

printf $"The number of safe levels is %i{safeLevels.Length} and the number with dapening is %i{dapenedSafeLevels.Length}"
