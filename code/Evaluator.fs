namespace Evaluator

open AST

type Value =
    | NumVal of float
    | UnitVal

module Evaluator =
    let rec evalExpr expr : Value =
        match expr with
        | Number v -> NumVal v
        | Seq exprs ->
            exprs
            |> List.map evalExpr
            |> ignore
            UnitVal

    let evalProgram (prog: Program) : Value =
        // Evaluate each expression in sequence
        prog
        |> List.map evalExpr
        |> List.last