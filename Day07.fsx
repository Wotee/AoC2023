let input = System.IO.File.ReadAllLines("inputs/07.txt")

type Label =
    | Joker = 1
    | Two = 2
    | Three = 3
    | Four = 4
    | Five = 5
    | Six = 6
    | Seven = 7
    | Eight = 8
    | Nine = 9
    | Ten = 10
    | Jack = 11
    | Queen = 12
    | King = 13
    | Ace = 14
module Label =
    let fromChar (c: char) =
        match c with
        | '2' -> Label.Two
        | '3' -> Label.Three
        | '4' -> Label.Four
        | '5' -> Label.Five
        | '6' -> Label.Six
        | '7' -> Label.Seven
        | '8' -> Label.Eight
        | '9' -> Label.Nine
        | 'T' -> Label.Ten
        | 'J' -> Label.Jack
        | 'Q' -> Label.Queen
        | 'K' -> Label.King
        | 'A' -> Label.Ace
        | _ -> failwith "Invalid card label"
    let fromCharP2 (c: char) =
        match c with
        | '2' -> Label.Two
        | '3' -> Label.Three
        | '4' -> Label.Four
        | '5' -> Label.Five
        | '6' -> Label.Six
        | '7' -> Label.Seven
        | '8' -> Label.Eight
        | '9' -> Label.Nine
        | 'T' -> Label.Ten
        | 'J' -> Label.Joker
        | 'Q' -> Label.Queen
        | 'K' -> Label.King
        | 'A' -> Label.Ace
        | _ -> failwith "Invalid card label"

let parseLine mapper (line: string) =
    let parts = line.Split([|' '|])
    let cards = parts[0] |> Seq.map mapper |> Seq.toArray
    let bid = parts[1] |> int
    cards, bid

type Type =
    | HighCard = 0
    | OnePair = 1
    | TwoPairs = 2
    | ThreeOfAKind = 3
    | FullHouse = 4
    | FourOfAKind = 5
    | FiveOfAKind = 6

let type' (cards: Label array) =
    match cards |> Array.sort with
    // Five
    | [|a;b;c;d;e|] when a = b && b = c && c = d && d = e -> Type.FiveOfAKind
    | [|j1; a; b; c; d|] when j1 = Label.Joker && a = b && b = c && c = d -> Type.FiveOfAKind
    | [|j1; j2; a; b; c;|] when j1 = Label.Joker && j2 = Label.Joker && a = b && b = c -> Type.FiveOfAKind
    | [|j1; j2; j3; a; b;|] when j1 = Label.Joker && j2 = Label.Joker && j3 = Label.Joker && a = b -> Type.FiveOfAKind
    | [|j1; j2; j3; j4; _;|] when j1 = Label.Joker && j2 = Label.Joker && j3 = Label.Joker && j4 = Label.Joker -> Type.FiveOfAKind
    // Four
    | [|a;b;c;d;_|] | [|_;d;c;b;a|] when a = b && b = c && c = d -> Type.FourOfAKind
    | [|j1;a;b;c;_|] | [|j1;_;a;b;c|] when j1 = Label.Joker && a = b && b = c -> Type.FourOfAKind
    | [|j1;j2;a;b;_|] | [|j1;j2;_;a;b|] when j1 = Label.Joker && j2 = Label.Joker && a = b -> Type.FourOfAKind
    | [|j1;j2;j3;_;_|] when j1 = Label.Joker && j2 = Label.Joker && j3 = Label.Joker -> Type.FourOfAKind
    // Full house
    | [|a;b;c;d;e|] | [|e;d;c;b;a|] when a = b && b = c && d = e -> Type.FullHouse
    | [|j1;a;b;c;d|] when j1 = Label.Joker && a = b && c = d -> Type.FullHouse
    | [|j1;j2;a;b;c|] when j1 = Label.Joker && j2 = Label.Joker && (a = b || b = c) -> Type.FullHouse
    // Three of a
    | [|a;b;c;_;_|] | [|_;a;b;c;_|] | [|_;_;a;b;c|] when a = b && b = c -> Type.ThreeOfAKind
    | [|j1;a;b;_;_|] | [|j1;_;a;b;_|] | [|j1;_;_;a;b|] when j1 = Label.Joker && a = b -> Type.ThreeOfAKind
    | [|j1;j2;_;_;_|] | [|j1;j2;_;_;_|] | [|j1;j2;_;_;_|] when j1 = Label.Joker && j2 = Label.Joker -> Type.ThreeOfAKind
    // Two pairs. With joker, we can have 3 of a kind always
    | [|a;b;c;d;_|] | [|a;b;_;c;d|] | [|_;a;b;c;d|] when a = b && c = d -> Type.TwoPairs
    // One pair
    | [|a;b;_;_;_|] | [|_;a;b;_;_|] | [|_;_;a;b;_|] | [|_;_;_;a;b|] when a = b -> Type.OnePair
    | [|j1;_;_;_;_|] when j1 = Label.Joker -> Type.OnePair
    | _ -> Type.HighCard

let sorter (lhs: Label array, _) (rhs: Label array, _) =
    let tlhs = type' lhs
    let trhs = type' rhs
    if tlhs = trhs then
        if lhs.[0] = rhs.[0] then
            if lhs.[1] = rhs.[1] then
                if lhs.[2] = rhs.[2] then
                    if lhs.[3] = rhs.[3] then
                        if lhs.[4] = rhs.[4] then
                            0
                        else
                            compare lhs.[4] rhs.[4]
                    else
                        compare lhs.[3] rhs.[3]
                else
                    compare lhs.[2] rhs.[2]
            else
                compare lhs.[1] rhs.[1]
        else
            compare lhs.[0] rhs.[0]
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
