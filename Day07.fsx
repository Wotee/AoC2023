let input = System.IO.File.ReadAllLines("inputs/07.txt")

type Label =
    | Joker
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace

module Label =
    let fromChar (c: char) =
        match c with
        | '2' -> Two
        | '3' -> Three
        | '4' -> Four
        | '5' -> Five
        | '6' -> Six
        | '7' -> Seven
        | '8' -> Eight
        | '9' -> Nine
        | 'T' -> Ten
        | 'J' -> Jack
        | 'Q' -> Queen
        | 'K' -> King
        | 'A' -> Ace
    let fromCharP2 (c: char) =
        match c with
        | '2' -> Two
        | '3' -> Three
        | '4' -> Four
        | '5' -> Five
        | '6' -> Six
        | '7' -> Seven
        | '8' -> Eight
        | '9' -> Nine
        | 'T' -> Ten
        | 'J' -> Joker
        | 'Q' -> Queen
        | 'K' -> King
        | 'A' -> Ace

let parseLine mapper (line: string) =
    let parts = line.Split([|' '|])
    let cards = parts[0] |> Seq.map mapper |> Seq.toArray
    let bid = parts[1] |> int
    cards, bid

type Type =
    | HighCard
    | OnePair
    | TwoPairs
    | ThreeOfAKind
    | FullHouse
    | FourOfAKind
    | FiveOfAKind

let type' (cards: Label array) =
    let sorted = cards |> Array.countBy id |> Array.sortByDescending snd |> Array.toList
    let mostCommonNotJoker =
        match sorted with
        | (Joker, 5)::_ -> Ace // Five of a kind
        | (Joker, _)::(label, _)::_ | (label,_)::_ -> label
    cards
    |> Array.map (fun c -> if c = Joker then mostCommonNotJoker else c)
    |> Array.countBy id |> Array.sortByDescending snd |> Array.toList
    |> function
    | (_, 5)::_ -> FiveOfAKind
    | (_, 4)::_ -> FourOfAKind
    | (_, 3)::(_, 2)::_ -> FullHouse
    | (_, 3)::_ -> ThreeOfAKind
    | (_, 2)::(_, 2)::_ -> TwoPairs
    | (_, 2)::_ -> OnePair
    | _ -> HighCard

let sorter (lhs: Label array, _) (rhs: Label array, _) =
    let tlhs = type' lhs
    let trhs = type' rhs
    if tlhs = trhs then
        Array.zip lhs rhs |> Array.pick (fun (l, r) -> match compare l r with 0 -> None | v -> Some v)
    else
        compare tlhs trhs

let solve parser =
    input
    |> Array.map (parseLine parser)
    |> Array.sortWith sorter
    |> Array.mapi (fun i (x, bid) -> (i+1)*bid)
    |> Array.sum

solve Label.fromChar |> printfn "Part 1: %d"
solve Label.fromCharP2 |> printfn "Part 2: %d"