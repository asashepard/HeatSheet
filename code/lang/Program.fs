open Parser
open System
open Combinator
open Evaluator

let usage () =
    printfn "Usage: dotnet run <file_name.hs>"
    Environment.Exit(1)

[<EntryPoint>]
let main args =
    if args.Length <> 1 then usage()

    let filePath = args[0]

    let fileContents =
        try
            System.IO.File.ReadAllText(filePath)
        with
        | :? System.IO.FileNotFoundException ->
            printfn "Error: File not found: %s" filePath
            Environment.Exit(1)
            ""
        | ex ->
            printfn "Error reading file: %s" ex.Message
            Environment.Exit(1)
            ""

    match parse fileContents with
    | Some p ->
        try
            let result = eval p
            printfn "%A" result
            0
        with
        | ex ->
            printfn "Runtime error: %s" ex.Message
            1
    | None ->
        printfn "Parse error: Invalid syntax in %s" filePath
        1
