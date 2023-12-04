let input = System.IO.File.ReadAllLines("inputs/04.txt")
#time

let getNumberOfWinningNumbersNew (x: string) =
    let split = x.Split([|"Card"; ":"; " "|], System.StringSplitOptions.RemoveEmptyEntries) |> Array.tail
    let winning, my = split |> Array.splitAt (split |> Array.findIndex ((=) "|"))
    Set.intersect (set winning) (set my)
    |> Set.count

let numberOfWinsPerCard = 
    input
    |> Array.map getNumberOfWinningNumbersNew

numberOfWinsPerCard
|> Array.sumBy (fun x -> pown 2 (x - 1))
|> printfn "Part 1: %i"

let cardCounts = Array.init input.Length (fun x -> x, 1) |> Map.ofArray

numberOfWinsPerCard
|> Array.indexed
|> Array.fold (fun cards (card, wins) ->
    let amountOfCards = Map.find card cards
    Array.init wins ((+) (card + 1))
    |> Array.fold (fun newC i ->
        newC |> Map.change i (Option.map ((+) (1 * amountOfCards)))
    ) cards
) cardCounts
|> Map.toSeq
|> Seq.sumBy snd
|> printfn "Part 2: %i"
#time