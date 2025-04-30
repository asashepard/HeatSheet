module Evaluator

open AST

type EvalState = {
    Athletes: Map<Identifier, AthleteDeclaration>
    Rosters: Map<Identifier, Set<Identifier>>
}

let emptyState = {
    Athletes = Map.empty
    Rosters = Map.empty
}

let evalStatement (stmt: Statement) : string =
    match stmt with
    | Athlete athlete ->
        sprintf "Created athlete %s with events: %s"
            athlete.Name (String.concat ", " athlete.Events)

    | Roster roster ->
        sprintf "Created roster %s" roster.Name

    | RosterAdd ra ->
        sprintf "Successfully added %s to roster %s" ra.Name ra.Roster

let eval (prog: Program) : string list = List.map evalStatement prog

