namespace HeatSheetMinimal

open System
open Parser.Parser  // No longer need AST or Evaluator

module Program =
    [<EntryPoint>]
    let main argv =
        let usage = "Usage: dotnet run <file.hs> or dotnet run \"<heatsheet program inline>\""
        match argv with
        | [| |] ->
            printfn "%s" usage
            0
        | [| single |] when single.EndsWith(".hs") || single.EndsWith(".txt") ->
            let text = System.IO.File.ReadAllText(single)
            try
                let declarations = parse text
                declarations |> List.iter (printfn "%A")
                0
            with ex ->
                printfn "Error: %s" ex.Message
                1
        | _ ->
            let inlineProg = String.Join(" ", argv)
            try
                let declarations = parse inlineProg
                declarations |> List.iter (printfn "%A")
                0
            with ex ->
                printfn "Error: %s" ex.Message
                1
