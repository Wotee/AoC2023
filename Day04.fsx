let input = System.IO.File.ReadAllLines("inputs/04.txt")
#time

let getNumberOfWinningNumbers (x: string) =
    let split = x.Split([|"Card "; ":"; "|"; " "|], System.StringSplitOptions.RemoveEmptyEntries) |> Array.tail |> Array.map int
    let winning, my = split |> Array.splitAt 10 // Ugly hack by looking at the input
    Set.intersect (winning |> Set.ofArray) (my |> Set.ofArray)
    |> Set.count

let numberOfWinsPerCard = 
    input
    |> Array.map getNumberOfWinningNumbers

numberOfWinsPerCard
|> Array.sumBy (fun x -> if x = 0 then 0 else [1..x-1] |> List.fold (fun acc _ -> acc * 2) 1)
|> printfn "Part 1: %i"

let cardCounts = Array.init input.Length (fun x -> x+1, 1) |> Map.ofArray

numberOfWinsPerCard
|> Array.indexed
|> Array.fold (fun cards (card, wins) ->
    let amountOfCards = Map.find (card + 1) cards
    Array.init wins (fun x -> card + 2 + x)
    |> Array.fold (fun newC i ->
        newC |> Map.change i (Option.map (fun x -> x + (1 * amountOfCards)))
    ) cards
) cardCounts
|> Map.toSeq
|> Seq.sumBy snd
|> printfn "Part 2: %i"
#time