let input = System.IO.File.ReadAllLines("inputs/04.txt")
#time

let numberOfWinsPerCard = 
    input
    |> Array.map (fun x ->
        let split = x.Split([|"Card"; ":"; " "|], System.StringSplitOptions.RemoveEmptyEntries) |> Array.tail
        let winning, my = split |> Array.splitAt (split |> Array.findIndex ((=) "|"))
        Set.intersect (set winning) (set my)
        |> Set.count
    )

numberOfWinsPerCard
|> Array.sumBy (fun x -> pown 2 (x - 1))
|> printfn "Part 1: %i"

numberOfWinsPerCard
|> Array.indexed
|> Array.fold (fun cards (card, wins) ->
    [ card + 1 .. card + wins]
    |> List.iter (fun j -> Array.tryItem j cards |> Option.iter (fun inst -> cards[j] <- inst + cards[card] ))
    cards
) (Array.create (Array.length numberOfWinsPerCard) 1)
|> Array.sum
|> printfn "Part 2: %i"
#time