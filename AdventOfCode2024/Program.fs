// For more information see https://aka.ms/fsharp-console-apps

open System.IO

let inputs = File.ReadAllLines "./sample.txt"

type Orientation =
    | North
    | South
    | East
    | West
    member this.turn =
        match this with
        | North -> East
        | East -> South
        | South -> West
        | West -> North

type Coordinate = Coordinate of (int * int)
with
    member this.value = let (Coordinate value) = this in value

    member this.x =
        let x, y = this.value
        x

    member this.y =
        let x, y = this.value
        y

    member this.move orientation =
        let x, y = this.value

        match orientation with
        | North -> Coordinate (x, y + 1)
        | East -> Coordinate (x + 1, y)
        | South -> Coordinate (x, y - 1)
        | West -> Coordinate (x - 1, y)
type Piece =
    | Hallway of Coordinate
    | Wall of Coordinate
    | Guard of Coordinate * Orientation
    static member parse legend position =
        match legend with
        | '.' -> Hallway position
        | '#' -> Wall position
        | '^' -> Guard (position , North)
        | '>' -> Guard (position, East)
        | 'v' -> Guard (position, South)
        | '<' -> Guard (position, West)
        | _ -> failwith "Unknown legend on map"

type Grid<'T> = Grid of 'T array array
with
    static member parse (rows: string array) (parser: char -> Coordinate -> 'T) =
        seq {
            for y = 0 to rows.Length - 1 do
                let values = rows[y].ToCharArray()
                values |> Array.mapi(fun x character -> parser character, Coordinate(x, y))
        } |> Seq.toArray

    member this.value = let (Grid value) = this in value

    member this.rows =
        this.value[0]

    member this.bounds =
        let min = Coordinate(0, 0)
        let max = Coordinate((this.value.Length - 1), (this.value[0].Length - 1))

        (min, max)

    member this.at (position: Coordinate) =
        this.value[position.y].[position.x]

    member this.find_first (lookup: 'T -> bool) =
        seq {
            for row in this.rows do
                for column in row do
                    if(lookup column) then yield column
        } |> Seq.head


let part_1 =
    let grid = Grid.parse inputs, Piece.parse
    0
