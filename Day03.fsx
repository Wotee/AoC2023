let input = System.IO.File.ReadAllLines("inputs/03.txt")
#time

let arr = input |> array2D

let getLocations f =
    arr
    |> Array2D.mapi (fun x y c -> if f c then Some (x, y, c) else None)
    |> Seq.cast<Option<int*int*char>>
    |> Seq.choose id
    |> Seq.toList

let rec buildNumberWithCoordinates (locations : list<int*int*char>) =
    [|
        match locations with
        | [] -> ()
        | (x1, y1, v1)::(x2, y2, v2)::(x3,y3,v3)::rest when x1 = x2 && x2 = x3 && y1 + 1 = y2 && y2 + 1 = y3->
            yield (int $"{v1}{v2}{v3}", [(x1, y1); (x2, y2); (x3, y3)])
            yield! buildNumberWithCoordinates rest
        | (x1, y1, v1)::(x2, y2, v2)::rest when x1 = x2 && y1 + 1 = y2 ->
            yield (int $"{v1}{v2}", [(x1, y1); (x2, y2)])
            yield! buildNumberWithCoordinates rest
        | (x1, y1, v1)::rest ->
            yield (int $"{v1}", [(x1, y1)])
            yield! buildNumberWithCoordinates rest
    |]

let getNeighbors (x, y) =
    [| x - 1, y - 1; x - 1, y; x - 1, y + 1; x, y - 1; x, y + 1; x + 1, y - 1; x + 1, y; x + 1, y + 1 |]

let maxX = Array2D.length1 arr - 1
let maxY = Array2D.length2 arr - 1
let locationCloseToSymbol (locations : (int*int) list) =
    locations
    |> List.toArray
    |> Array.collect getNeighbors
    |> Array.distinct
    |> Array.filter (fun (x, y) -> x >= 0 && x <= maxX && y >= 0 && y <= maxY)
    |> Array.exists (fun (x, y) ->
        let char = Array2D.get arr x y
        not (System.Char.IsDigit char || char = '.')
    )

let numbers = getLocations System.Char.IsDigit |> buildNumberWithCoordinates

numbers
|> Array.filter (snd >> locationCloseToSymbol)
|> Array.sumBy fst
|> printfn "Part 1: %i"

let numbersMapped =
    numbers
    |> Array.collect(fun (value, locations) -> locations |> List.toArray |> Array.map (fun (x, y) -> ((x, y), value)))
    |> Map.ofArray

let getGearRatios (x, y, _) =
    match getNeighbors (x,y) |> Array.choose (fun coord -> Map.tryFind coord numbersMapped) |> Array.distinct with
    | [|a;b|] -> a*b
    | _ -> 0

getLocations ((=) '*')
|> List.sumBy getGearRatios
|> printfn "Part 2: %i"
#time