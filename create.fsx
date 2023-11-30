#r "nuget: FsHttp"
open FsHttp
let day = System.DateTime.Now.Day
let filename = $"inputs/%02i{day}.txt"
let url = $"https://adventofcode.com/2023/day/%i{day}/input"
let session = fsi.CommandLineArgs.[1]

// Write the template code reading the input
let templateCode =
    $"""//let input = System.IO.File.ReadAllLines("{filename}")
let input = System.IO.File.ReadAllLines("inputs/temp.txt")


"""

System.IO.File.WriteAllText($"Day%02i{day}.fsx", templateCode)

// Get the input
http {
    GET url
    Cookie "session" session
}
|> Request.send
|> Response.saveFile filename
