let input = System.IO.File.ReadAllLines("inputs/01.txt")
#time

let getFirstAndLast (x: string) =
    10 * (x |> Seq.find System.Char.IsDigit |> System.Convert.ToInt32)
    + (x |> Seq.findBack System.Char.IsDigit |> System.Convert.ToInt32)

let parseP2 (x: string) =
    x
        .Replace("one", "one1one")
        .Replace("two", "two2two")
        .Replace("three", "three3three")
        .Replace("four", "four4four")
        .Replace("five", "five5five")
        .Replace("six", "six6six")
        .Replace("seven", "seven7seven")
        .Replace("eight", "eight8eight")
        .Replace("nine", "nine9nine")

input |> Array.sumBy getFirstAndLast |> printfn "Part 1: %d"

input |> Array.sumBy (parseP2 >> getFirstAndLast) |> printfn "Part 2: %d"
#time