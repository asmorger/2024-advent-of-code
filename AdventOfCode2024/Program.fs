open System.IO
let inputs = File.ReadAllLines "./input.txt"

let find_guard (arr: char [,]) =
    let is_guard c = c = '^' || c = '>' || c = 'v' || c = '<'

    let rec go x y =
          if   y >= arr.GetLength 1 then None
          elif x >= arr.GetLength 0 then go 0 (y+1)
          elif is_guard arr[x,y]  then Some (x,y)
          else go (x+1) y
    go 0 0

let guard_take_action (guard: int * int) (grid: char [,]) =
    let x, y = guard

    let orientation = grid[x, y]

    let next =
        match orientation with
        | '^' -> (x - 1, y)
        | '>' -> (x, y + 1)
        | 'v' -> (x + 1, y)
        | '<' -> (x, y - 1)
        | _ -> failwith "unknown"

    let next_x, next_y = next
    let next_char =
        if next_x >= grid.GetLength 0 then None
        elif next_y >= grid.GetLength 1 then None
        elif next_x = -1 || next_y = -1 then None
        else Some(grid[next_x, next_y])

    match next_char with
    | Some(char)  when char = '.' ->
        grid.SetValue(orientation, next_x, next_y)
        grid.SetValue('X', x, y)
        Some (1, grid, next)
    | Some(char)  when char = 'X' ->
        grid.SetValue(orientation, next_x, next_y)
        grid.SetValue('X', x, y)
        Some (0, grid, next)
    | Some(char) when char ='#' ->
        let rotation =
            match orientation with
            | '^' -> '>'
            | '>' -> 'v'
            | 'v' -> '<'
            | '<' -> '^'
            | _ -> failwith "unknown orientation"

        grid.SetValue(rotation, x, y)
        Some (0, grid, guard)
    | _ -> None

let move_guard (guard: int * int) (grid: char [,]) =
    let mutable current_grid = Some grid
    let mutable current_guard = guard
    let mutable count = 1

    while current_grid.IsSome do
        let result = guard_take_action current_guard grid

        match result with
        | Some(movement, grid, guard) ->
            count <- count + movement
            current_guard <- guard
            current_grid <- Some grid

            // printfn "%A" grid
            // printfn "-------------------------------"
        | None ->
            current_grid <- None

    count

let part_1 =
    let grid = inputs |> Array.map(_.ToCharArray()) |> array2D
    let guard = find_guard grid |> Option.defaultValue (0, 0)

    let action = move_guard guard grid
    printfn $"Result of part 1 is %i{action}"

part_1
