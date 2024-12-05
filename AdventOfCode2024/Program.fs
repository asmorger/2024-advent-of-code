// For more information see https://aka.ms/fsharp-console-apps

open System.IO
open System.Text.RegularExpressions

let input = File.ReadAllLines "./input.txt"

let explodeGrid () =
    let relativeDirections = [-1; 0; 1]

    relativeDirections
    |> List.map(fun y -> relativeDirections |> List.map(fun x -> (x, y)))
    |> List.concat
    |> List.filter(fun x -> x <> (0,0))

let doesMatchChar
    (grid: char array array)
    (x: int)
    (y: int)
    (target: char)
    : bool
    =
    if(y < 0 || y >= grid.Length || x < 0 || x >= grid[0].Length) then false
    else grid[y][x] = target


let countWordsInStraightLine
    (grid: char array array)
    (xStart: int)
    (yStart: int)
    (target: char array)
    : int
    =
    let allRelativePositions =
        explodeGrid()
        |> List.map(fun (x,y) ->
            seq { 0..target.Length - 1 }
            |> Seq.map(fun v -> (x * v, y * v)))

    allRelativePositions
    |> List.map(fun allCoordinates ->
        allCoordinates
        |> List.ofSeq
        |> List.mapi(fun i (x, y) -> (i, x, y))
        |> List.forall(fun (i, x, y) ->
            doesMatchChar
                grid
                (xStart + x)
                (yStart + y)
                target[i]
          )
      )

    |> List.filter id
    |> List.length

let part1 (input : string array) =
    let grid = input |> Array.map(_.ToCharArray())
    let mutable count = 0

    for y = 0 to grid.Length - 1 do
        for x = 0 to grid[0].Length - 1 do
            let foundWords =
                countWordsInStraightLine
                    grid
                    x
                    y
                    ("XMAS".ToCharArray())

            count <- count + foundWords
    count

let result = part1 input
printfn $"The total number of words is %i{result}"

let isXMas (grid: char array array) (x: int) (y:int)  : bool =
    let isCharEqual = doesMatchChar grid

    if not (isCharEqual x y 'A') then false
    else
        let left =(
            (isCharEqual (x  - 1) (y - 1) 'M' && isCharEqual (x + 1) (y + 1) 'S')
            ||
            (isCharEqual (x  - 1) (y - 1) 'S' && isCharEqual (x + 1) (y + 1) 'M')
        )


        let right =(
            (isCharEqual (x  + 1) (y - 1) 'M' && isCharEqual (x - 1) (y + 1) 'S')
            ||
            (isCharEqual (x  + 1) (y - 1) 'S' && isCharEqual (x - 1) (y + 1) 'M')
        )

        left && right

let part2 (input : string array) =
    let grid = input |> Array.map(_.ToCharArray())
    let mutable count = 0

    for y = 0 to grid.Length - 1 do
        for x = 0 to grid[0].Length - 1 do
            let foundWords =
                if (isXMas grid x y) then 1
                else 0

            count <- count + foundWords
    count


let result2 = part2 input

printfn $"The total number of XMAS is %i{result2}"
