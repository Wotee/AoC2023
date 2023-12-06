let input = System.IO.File.ReadAllLines("inputs/06.txt")
#time
let parse mapper chooser = input |> Array.map  (mapper >> Array.tail >> Array.map int64) |> chooser 
let solve (t, d) = [|1L..t-1L|] |> Array.map (fun ms -> ms * (t - ms)) |> Array.filter ((<) d) |> Array.length
parse (fun (s: string) -> s.Split(" ", System.StringSplitOptions.RemoveEmptyEntries)) (fun ([|a;b|]) -> Array.zip a b) |> Array.map solve |> Array.reduce (*) |> printfn "Part 1: %i"
parse (fun (s: string) -> s.Replace(" ", "").Split(":")) (fun ([|a;b|]) -> a[0], b[0]) |> solve |> printfn "Part 2: %i"
#time