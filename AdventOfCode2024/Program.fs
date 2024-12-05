// For more information see https://aka.ms/fsharp-console-apps

open System.IO
open System.Reflection

let inputs = File.ReadAllLines "./input.txt"
let separatorIndex = inputs |> Array.findIndex(fun x -> x.Length = 0)
let sections = inputs |> Array.splitAt separatorIndex
let page_ordering_rules_source, page_numbers_of_updates_source = sections

let page_number_updates =
    page_numbers_of_updates_source
    |> Array.filter(fun x -> x.Length > 0)
    |> Array.map(fun x -> x.Split ',' |> Array.map(int) |> Array.toList)

let page_ordering_rules =
    page_ordering_rules_source
    |> Array.map(fun x ->
        let values = x.Split '|'
        (int(values[0]), int(values[1]))
      )

    |> Set

let matches_rule (rule: int * int) (update: int list) : bool =
    let target, after = rule
    let targetIndex = update |> List.findIndex(fun x -> x = target)
    let afterIndex = update |> List.findIndex(fun x -> x = after)

    targetIndex < afterIndex

let is_update_in_correct_order (update: int list) : bool =
    let all_pairs_in_update = List.allPairs update update |> Set
    let matchedRules = all_pairs_in_update |> Set.intersect page_ordering_rules
    let correctRules = matchedRules |> Set.filter(fun rule -> matches_rule rule update)

    matchedRules.Count = correctRules.Count

let part_1 =
    page_number_updates
    |> Array.filter(is_update_in_correct_order)
    |> Array.map(fun arr -> arr[arr.Length / 2])
    |> Array.sum

let result_1 = part_1

printfn $"The number of sum of part 1 is %i{result_1}"

let update_to_match_rule  (update: int list) (rule: int * int) : int list =
    let before, after = rule

    let beforeIndex = update |> List.findIndex(fun x -> x = before)
    let afterIndex = update |> List.findIndex(fun x -> x = after)

    let targetIndex = if(afterIndex - 1) < 0 then 0 else afterIndex - 1

    update
    |> List.removeAt beforeIndex
    |> List.insertAt targetIndex before

let rec reorder_the_update (update: int list) : int list =
    let all_pairs_in_update = List.allPairs update update |> Set
    let matchedRules = all_pairs_in_update |> Set.intersect page_ordering_rules
    let incorrectRules = matchedRules |> Set.filter(fun rule -> matches_rule rule update |> not)

    if incorrectRules.Count = 0 then update
    else
        let updated = update_to_match_rule update incorrectRules.MinimumElement
        reorder_the_update updated

let part_2 =
    let updated =
        page_number_updates
        |> Array.filter(fun x -> is_update_in_correct_order x |> not)
        |> Array.map(fun x -> reorder_the_update x)

    updated
    |> Array.map(fun arr -> arr[arr.Length / 2])
    |> Array.sum

let result_2 = part_2
printfn $"The number of sum of part 2 is %i{result_2}"
