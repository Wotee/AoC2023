let input = System.IO.File.ReadAllLines("inputs/10.txt")
#time

type Dir = Up of int*int*char | Down of int*int*char | Left of int*int*char | Right of int*int*char
let arr = input |> array2D
let len1 = arr |> Array2D.length1
let len2 = arr |> Array2D.length2

let start =
    arr
    |> Array2D.mapi(fun y x v -> if v = 'S' then Some (y, x) else None)
    |> Seq.cast<Option<int*int>>
    |> Seq.pick id

let tryFind (y, x) (arr: 'a[,]) =
    if y < 0 || y >= len1 || x < 0 || x >= len2 then None
    else Some (arr.[y, x])

let validStartDirs (y, x) =
    [|
        let (yu, xu) as u = y-1, x
        let (yd, xd) as d = y+1, x
        let (yr, xr) as r = y, x+1
        let (yl, xl) as l = y, x-1
        let up = arr |> tryFind u
        match up with
        | Some '|'
        | Some 'F'
        | Some '7' -> yield Up (yu, xu, arr[yu, xu])
        | _ -> ()
        let down = arr |> tryFind d
        match down with
        | Some '|'
        | Some 'J'
        | Some 'L' -> yield Down (yd, xd, arr[yd, xd])
        | _ -> ()
        let left = arr |> tryFind l
        match left with
        | Some '-'
        | Some 'F'
        | Some 'L' -> yield Left (yl, xl, arr[yl, xl])
        | _ -> ()
        let right = arr |> tryFind r
        match right with
        | Some '-'
        | Some '7'
        | Some 'J' -> yield Right (yr, xr, arr[yr, xr])
        | _ -> ()
    |]

let next (dir: Dir) =
    match dir with
    | Down (y, x, c) ->
        match c with
        | '|' -> Down (y+1, x, arr[y+1, x])
        | 'L' -> Right (y, x+1, arr[y, x+1])
        | 'J' -> Left (y, x-1, arr[y, x-1])
        | x -> failwithf "Invalid direction Down %c" x
    | Up (y,x, c) ->
        match c with
        | '|' -> Up (y-1, x, arr[y-1, x])
        | 'F' -> Right (y, x+1, arr[y, x+1])
        | '7' -> Left (y, x-1, arr[y, x-1])
        | x -> failwithf "Invalid direction Up %c" x
    | Left (y, x, c) ->
        match c with
        | '-' -> Left (y, x-1, arr[y, x-1])
        | 'F' -> Down (y+1, x, arr[y+1, x])
        | 'L' -> Up (y-1, x, arr[y-1, x])
        | x -> failwithf "Invalid direction Left %c" x
    | Right (y,x,c) ->
        match c with
        | '-' -> Right (y, x+1, arr[y, x+1])
        | '7' -> Down (y+1, x, arr[y+1, x])
        | 'J' -> Up (y-1, x, arr[y-1, x])
        | x -> failwithf "Invalid direction Right %c" x

let rec step current =
    seq {
        let n = current |> next
        match n with
        | Up (_, _, c) | Down (_, _, c) | Left (_, _, c) | Right (_, _, c) ->
            match c with
            | 'S' ->
                yield n
            | _ ->
                yield n
                yield! step n
    }
 

let s = start |> validStartDirs |> Array.head

let ans = s |> step |> Seq.toArray |> Array.append [|s|]

(ans.Length)/2 |> printfn "Part 1: %i"

let loop = ans |> Array.map (function Up (y, x, _) | Down (y, x, _) | Left (y, x, _) | Right (y, x, _) -> y, x)

let shoelace (s: (int*int) array) =
    let y, x = s |> Array.unzip
    [|1..s.Length|]
    |> Array.sumBy (fun i -> (x.[i%s.Length] * y.[(i-1)%s.Length]) - (x.[(i-1)%s.Length] * y.[i%s.Length]))
    |> abs
    |> float
    |> (*) 0.5

let picksTheorem loopSize (area: float) =
    area - (0.5 * float loopSize) + 1.
    |> int

let solve = 
    shoelace >> picksTheorem loop.Length

loop |> solve |> printfn "Part 2: %i"
#time