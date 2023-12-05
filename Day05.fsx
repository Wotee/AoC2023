let input = System.IO.File.ReadAllText("inputs/05.txt")

let parts = input.Split("\n\n")

let seeds = parts[0].Split(" ")[1..] |> Array.map int64

let buildRange (s: string) =
    let arr = s.Split("\n", System.StringSplitOptions.RemoveEmptyEntries)[1..]
    arr
    |> Array.map (fun s ->
        match s.Split(" ") |> Array.map int64 with
        | [|destStart; sourceStart; length|] ->
            sourceStart, sourceStart+length-1L, destStart, destStart+length-1L
    )

let seedToSoil = parts.[1] |> buildRange

let soilToFertilizer = parts.[2] |> buildRange

let fertilizerToWater = parts.[3] |> buildRange

let waterToLight = parts.[4] |> buildRange

let lightToTemperature = parts.[5] |> buildRange

let temperatureToHumidity = parts.[6] |> buildRange

let humidityToLocation = parts.[7] |> buildRange

let findNext (values: (int64*int64*int64*int64) array) (i: int64) =
    values
    |> Array.tryFind (fun (start, end_, _, _) -> start <= i && i <= end_)
    |> Option.map (fun (start, end_, destStart, destEnd) -> destStart + i - start)
    |> Option.defaultValue i

let find = findNext seedToSoil >> findNext soilToFertilizer >> findNext fertilizerToWater >> findNext waterToLight >> findNext lightToTemperature >> findNext temperatureToHumidity >> findNext humidityToLocation

let result = seeds |> Array.map find |> Array.min
result
|> printfn "%A"

seeds

let seeds2 = seeds |> Array.chunkBySize 2 |> Array.collect (fun ([|a; b|]) -> [|a..a+b|])

seeds2 |> Array.map find |> Array.min |> printfn "%A"
