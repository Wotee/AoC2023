#nowarn "25"
let input = System.IO.File.ReadAllLines("inputs/08.txt")
#time
let steps = input[0] |> Seq.map (function 'L' -> fst | 'R' -> snd) |> Seq.toArray
let stepsLength = steps.Length
let directions =
    input[2..]
    |> Array.map (_.Split([|' '; '='; '('; ')'; ','|], System.StringSplitOptions.RemoveEmptyEntries))
    |> Array.map (fun parts -> parts.[0], (parts.[1], parts.[2]))
    |> Map.ofArray

let rec calculateNeededSteps i location  =
    let stepsTaken = i + 1L
    let nextLocation = steps[int i%stepsLength] directions[location]
    if nextLocation.EndsWith("Z") then
        stepsTaken
    else
        calculateNeededSteps stepsTaken nextLocation 
let rec gcd a b = if b = 0L then a else gcd b (a % b)
let lcm = Seq.reduce (fun acc x -> acc * x / (gcd acc x))
let neededSteps = 
    directions
    |> Map.filter (fun key _ -> key.EndsWith("A"))
    |> Map.map (fun key _ -> calculateNeededSteps 0L key)

neededSteps["AAA"] |> printfn "Part 1: %i"
neededSteps |> Map.values |> lcm |> printfn "Part 2: %i"
#time