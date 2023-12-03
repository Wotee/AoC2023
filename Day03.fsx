let input = System.IO.File.ReadAllLines("inputs/03.txt")
#time

let arr =
    input
    |> Array.map Seq.toArray
    |> array2D

let getLocations f =
    arr
    |> Array2D.mapi (fun x y c -> if f c then Some (x, y, c) else None)
    |> Seq.cast<Option<int*int*char>>
    |> Seq.choose id
    |> Seq.toList

let rec buildNumber (locations : list<int*int*char>) =
    [
        match locations with
        | [] -> ()
        | (x1, y1, v1)::(x2, y2, v2)::(x3,y3,v3)::rest when x1 = x2 && x2 = x3 && y1 + 1 = y2 && y2 + 1 = y3->
            yield (int $"{v1}{v2}{v3}", [(x1, y1); (x2, y2); (x3, y3)])
            yield! buildNumber rest
        | (x1, y1, v1)::(x2, y2, v2)::rest when x1 = x2 && y1 + 1 = y2 ->
            yield (int $"{v1}{v2}", [(x1, y1); (x2, y2)])
            yield! buildNumber rest
        | (x1, y1, v1)::rest ->
            yield (int $"{v1}", [(x1, y1)])
            yield! buildNumber rest
    ]

let getNeighbors (x, y) =
    [ x - 1, y - 1; x - 1, y; x - 1, y + 1; x, y - 1; x, y + 1; x + 1, y - 1; x + 1, y; x + 1, y + 1 ]



let maxX = Array2D.length1 arr - 1
let maxY = Array2D.length2 arr - 1
let filter (locations : (int*int) list) =
    let validNeighbors =
        locations
        |> List.collect getNeighbors
        |> List.distinct
        |> List.filter (fun (x, y) -> x >= 0 && x <= maxX && y >= 0 && y <= maxY)
    validNeighbors
    |> List.exists (fun (x, y) ->
        let char = Array2D.get arr x y
        not (System.Char.IsDigit char || char = '.')
    )

let numbers = getLocations System.Char.IsDigit |> buildNumber

numbers
|> List.filter (fun (_, locations) -> filter locations)
|> List.sumBy fst
|> printfn "Part 1: %i"

let gearLocations = getLocations (fun c -> c = '*')

let numbersMapped =
    numbers
    |> List.collect(fun (value, locations) -> locations |> List.map (fun (x, y) -> ((x, y), value)))
    |> Map.ofList

let getGearRatios (x, y, _) =
    let gearNeighbors = getNeighbors (x, y)
    let gears = gearNeighbors |> List.choose (fun coord -> Map.tryFind coord numbersMapped) |> List.distinct
    match gears with
    | a::b::[] -> a*b
    | _ -> 0

gearLocations
|> List.sumBy getGearRatios
|> printfn "Part 2: %i"
#time