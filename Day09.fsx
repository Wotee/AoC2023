let input = System.IO.File.ReadAllLines("inputs/09.txt")
#time
let data = input |> Array.map (fun x -> x.Split(' ') |> Seq.map int |> Seq.toArray)

let rec diffs (a: int array) =
    [|
        yield a
        let new' = a |> Array.pairwise |> Array.map (fun (a, b) -> b - a)
        if new' |> Array.forall ((=) 0) then yield new'
        else yield! diffs new'
    |]
let solve f (a : int array array) =
    a |> Array.map (f >> diffs >> Array.sumBy Array.last) |> Array.sum

data |> solve id |> printfn "Part 1: %d"
data |> solve Array.rev |> printfn "Part 2: %d"
#time