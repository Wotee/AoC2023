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
    match cards |> Array.sort with
    | [|a;b;c;d;e|] when a = b && b = c && c = d && d = e -> FiveOfAKind
    | [|j1; a; b; c; d|] when j1 = Joker && a = b && b = c && c = d -> FiveOfAKind
    | [|j1; j2; a; b; c;|] when j1 = Joker && j2 = Joker && a = b && b = c -> FiveOfAKind
    | [|j1; j2; j3; a; b;|] when j1 = Joker && j2 = Joker && j3 = Joker && a = b -> FiveOfAKind
    | [|j1; j2; j3; j4; _;|] when j1 = Joker && j2 = Joker && j3 = Joker && j4 = Joker -> FiveOfAKind
    | [|a;b;c;d;_|] | [|_;d;c;b;a|] when a = b && b = c && c = d -> FourOfAKind
    | [|j1;a;b;c;_|] | [|j1;_;a;b;c|] when j1 = Joker && a = b && b = c -> FourOfAKind
    | [|j1;j2;a;b;_|] | [|j1;j2;_;a;b|] when j1 = Joker && j2 = Joker && a = b -> FourOfAKind
    | [|j1;j2;j3;_;_|] when j1 = Joker && j2 = Joker && j3 = Joker -> FourOfAKind
    | [|a;b;c;d;e|] | [|e;d;c;b;a|] when a = b && b = c && d = e -> FullHouse
    | [|j1;a;b;c;d|] when j1 = Joker && a = b && c = d -> FullHouse
    | [|j1;j2;a;b;c|] when j1 = Joker && j2 = Joker && (a = b || b = c) -> FullHouse
    | [|a;b;c;_;_|] | [|_;a;b;c;_|] | [|_;_;a;b;c|] when a = b && b = c -> ThreeOfAKind
    | [|j1;a;b;_;_|] | [|j1;_;a;b;_|] | [|j1;_;_;a;b|] when j1 = Joker && a = b -> ThreeOfAKind
    | [|j1;j2;_;_;_|] | [|j1;j2;_;_;_|] | [|j1;j2;_;_;_|] when j1 = Joker && j2 = Joker -> ThreeOfAKind
    | [|a;b;c;d;_|] | [|a;b;_;c;d|] | [|_;a;b;c;d|] when a = b && c = d -> TwoPairs
    | [|a;b;_;_;_|] | [|_;a;b;_;_|] | [|_;_;a;b;_|] | [|_;_;_;a;b|] when a = b -> OnePair
    | [|j1;_;_;_;_|] when j1 = Joker -> OnePair
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
