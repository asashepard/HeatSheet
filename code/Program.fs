open Parser
open System
open Combinator
open Evaluator


let usage() =
    printfn "Usage: dotnet run <file_name.hs>"
    exit 1  

[<EntryPoint>]
let main args =
    if args.Length <> 1 then usage()
    let fileContents = System.IO.File.ReadAllText(args[0])
    let result = parse fileContents
    //printfn "%A" result
    match result with
    | Some p -> eval p
    | None -> usage()
