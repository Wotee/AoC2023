let input = System.IO.File.ReadAllLines("inputs/11.txt")
#time

let rec transpose =
    function
    | []::_ -> []
    | xs -> List.map List.head xs :: transpose (List.map List.tail xs)

let manhattan (x1,y1) (x2,y2) = abs (x1 - x2) + abs (y1 - y2)

let originalPoints = 
    input |> array2D |> Array2D.mapi (fun y x v -> if v = '#' then Some (int64 x, int64 y) else None) |> Seq.cast<(int64*int64) option> |> Seq.choose id |> Seq.toArray

let emptyRows =
    input
    |> Array.mapi (fun i v -> if v |> Seq.contains '#' then None else Some i)
    |> Array.choose id

let emptyCols =
    input
    |> Array.map (fun s -> s |> Seq.toArray)
    |> Array.toList
    |> List.map Seq.toList
    |> transpose
    |> List.toArray
    |> Array.mapi (fun i v -> if v |> Seq.contains '#' then None else Some i)
    |> Array.choose id

let expand factor points =
    points
    |> Array.map (fun (x, y) ->
        let largerThan = emptyCols |> Array.takeWhile (fun i -> int64 i < x) |> Array.length |> int64
        (x + (largerThan*(factor-1L)), y))
    |> Array.map (fun (x, y) ->
        let largerThan = emptyRows |> Array.takeWhile (fun i -> int64 i < y) |> Array.length |> int64
        (x, y + (largerThan*(factor-1L)))
    )

let newPairs (p: 'a array) =
    [|
        for i in 0..p.Length-1 do
            for j in i+1..p.Length-1 do
                yield (p[i], p[j])
    |]

let solve i = expand i >> newPairs >> Array.sumBy (fun (a,b) -> (manhattan a b))

originalPoints |> solve 2L |> printfn "Part 1: %d"
originalPoints |> solve 1000000L |> printfn "Part 1: %d"
#time