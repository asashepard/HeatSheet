module Evaluator

open AST
open LatexCreator
open System

type EvalState = {
    Athletes: Map<Identifier, AthleteDeclaration>
    Rosters: Map<Identifier, Set<Identifier>>
    Meets: Map<Identifier, MeetDeclaration>
}

let emptyState = {
    Athletes = Map.empty
    Rosters = Map.empty
    Meets = Map.empty
}

let addAthlete (state: EvalState) (a: AthleteDeclaration) =
    { state with Athletes = state.Athletes.Add(a.Name, a) }

let addRoster (state: EvalState) (r: RosterDeclaration) =
    let athletes = Set.ofList r.Athletes
    { state with Rosters = state.Rosters.Add(r.Name, athletes) }


let addToRoster (state: EvalState) (ra: RosterAdd) =
    match Map.tryFind ra.Name state.Athletes, Map.tryFind ra.Roster state.Rosters with
    | Some _, Some members ->
        let updated = members.Add ra.Name
        { state with Rosters = state.Rosters.Add(ra.Roster, updated) }
    | _ -> state

let addMeet (state: EvalState)(m: MeetDeclaration) =
    {state with Meets = state.Meets.Add(m.Name, m)}

let addToMeet (state: EvalState) (ma: MeetAdd) =
    match Map.tryFind ma.Meet state.Meets with
    | Some meet ->
        if List.contains ma.TeamToAdd meet.Teams then state
        else
            let updatedMeet = { meet with Teams = ma.TeamToAdd :: meet.Teams }
            { state with Meets = state.Meets.Add(ma.Meet, updatedMeet) }
    | None -> state // probably should error here


// ---------------------- LaTeX Formatting ----------------------

let formatTime = function
    | Float f -> sprintf "%.2f" f
    | MinuteTime (m, s) -> sprintf "%.0f:%.2f" m s

let formatAthleteCells (a: AthleteDeclaration) : string * string * string =
    let events = String.concat ", " a.Events
    let prs =
        a.PRs
        |> List.map (fun pr -> sprintf "%s: %s" pr.Event (formatTime pr.Time))
        |> String.concat ", "
    a.Name, events, prs

let renderAthleteRow (state: EvalState) (name: Identifier) : string =
    match Map.tryFind name state.Athletes with
    | Some a ->
        let n, e, p = formatAthleteCells a
        sprintf "%s & %s & %s \\\\" n e p
    | None ->
        sprintf "%s & UNKNOWN & UNKNOWN \\\\" name

let buildRosterTable (rows: string list) : string =
    let header = "\\toprule\n\\textbf{Name} & \\textbf{Events} & \\textbf{PRs} \\\\\n\\midrule"
    let body = String.concat "\n" rows
    String.concat "\n" [
        "\\begin{tabularx}{\\textwidth}{lXl}"
        header
        body
        "\\bottomrule"
        "\\end{tabularx}"
    ]


let buildLatexDocument (table: string) (roster: string) : string =
    String.concat "\n" [
        "\\documentclass[11pt]{article}"
        "\\usepackage[margin=1in]{geometry}"
        "\\usepackage{booktabs}"
        "\\usepackage{longtable}"
        "\\usepackage{tabularx}"
        "\\usepackage{booktabs}"
        "\\usepackage{siunitx}"
        "\\newcolumntype{L}{>{\\raggedright\\arraybackslash}X}"
        "\\begin{document}"
        $"\\section*{{Roster: {roster}}}"
        table
        "\\end{document}"
    ]

// ---------------------- Generation + Evaluation ----------------------

let generateLatex (state: EvalState) (roster: Identifier) : string option =
    match Map.tryFind roster state.Rosters with
    | None -> None
    | Some members ->
        let rows = members |> Set.toList |> List.map (renderAthleteRow state)
        let table = buildRosterTable rows
        let tex = buildLatexDocument table roster
        Some (runPdfLatex tex "." (roster + "_roster"))

let eval (prog: Program) : unit =
    let finalState, _ =
        List.fold (fun (state, lastPdf) stmt ->
            match stmt with
            | Athlete a    -> addAthlete state a, lastPdf
            | Roster r     -> addRoster state r, lastPdf
            | RosterAdd ra -> addToRoster state ra, lastPdf
            | RosterShow rs ->
                match generateLatex state rs.RosterToShowName with
                | Some path ->
                    printfn "PDF generated: %s" path
                    state, Some path
                | None ->
                    printfn "Roster %s not found" rs.RosterToShowName
                    state, lastPdf
            | Meet m -> addMeet state m, lastPdf
            | MeetAdd ma -> addToMeet state ma, lastPdf
            | Optimize o -> state, lastPdf

        ) (emptyState, None) prog
    ()
