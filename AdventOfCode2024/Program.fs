// For more information see https://aka.ms/fsharp-console-apps

open System.IO
open System.Text.RegularExpressions

let input = File.ReadAllText "./input.txt"
let sample ="xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
let regex = Regex(@"mul\([\d]+,[\d]+\)")

let testMatches = regex.Count sample

printfn $"The number of matches is %i{testMatches}"

let parse (expression:string) =
    let clean = expression.Replace("mul(", "").Replace(")", "")
    
    clean.Split ','
    |> Array.map(int)
    |> Array.fold (*) 1
    
let result =
    regex.Matches(input)
    |> Seq.map(_.Value)
    |> Seq.map(parse)
    |> Seq.sum
    
printfn $"The result is %i{result}"

let split (input:string) =
    seq{
        let dos = input.Split "do()"
        
        for section in dos do
            let stop = section.IndexOf "don't()"
            
            if stop > 0 then section.Substring(0, stop)
            else section
    }

let cleanedInput = input |> split |> String.concat ""
let result2 =
    regex.Matches(cleanedInput)
        |> Seq.map(_.Value)
        |> Seq.map(parse)
        |> Seq.sum

printfn $"The updated result is %i{result2}"
