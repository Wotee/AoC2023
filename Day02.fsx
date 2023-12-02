let input = System.IO.File.ReadAllLines("inputs/02.txt")
#time

let red, green, blue = 12, 13, 14

let parse (game: string) =
    let values = game.Split([|"Game"; ":"; ","; ";"|], (System.StringSplitOptions.TrimEntries ||| System.StringSplitOptions.RemoveEmptyEntries))
    let gameId = int values.[0]
    let largestDieValues =
        values.[1..]
        |> Array.map (fun x -> x.Split(" ") |> fun y -> int y[0], y[1])
        |> Array.groupBy snd
        |> Array.map (fun (color, groups) -> color, Array.maxBy fst groups |> fst)
    gameId, largestDieValues

let parsed = input |> Array.map parse

let getValidGameIds (gameId, groups) =
    groups
    |> Array.forall (fun (color, count) ->
        match color with
        | "red" -> count <= red
        | "green" -> count <= green
        | "blue" -> count <= blue
        | _ -> false)
    |> function
    | true -> gameId
    | false -> 0

parsed
|> Array.sumBy getValidGameIds
|> printfn "Part 1: %i"

let multiplyDieValues (_, groups) =
    groups
    |> Array.map snd
    |> Array.reduce (*)

parsed
|> Array.sumBy multiplyDieValues
|> printfn "Part 2: %i"
#time