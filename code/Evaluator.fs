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


// ---------------------- Optimizer ----------------------
 
// athlete assignment to event (athlete * event)
type Assignment = (Identifier * Identifier) list

let scoreAssignment (assignment: Assignment) (state: EvalState) (meet: MeetDeclaration) (yourRoster: Set<Identifier>) (opponents: AthleteDeclaration list) : int =
    // Build full event entry list
    let fullEntries =
        let yourEntries = assignment
        let opponentEntries =
            opponents
            |> List.collect (fun a ->
                a.PRs
                |> List.map (fun pr -> (a.Name, pr.Event, pr.Time))
            )
        // Expand your entries with actual PRs
        let yourEntriesWithPRs =
            yourEntries
            |> List.choose (fun (athlete, event) ->
                Map.tryFind athlete state.Athletes
                |> Option.bind (fun a ->
                    a.PRs
                    |> List.tryFind (fun pr -> pr.Event = event)
                    |> Option.map (fun pr -> (athlete, event, pr.Time))
                )
            )
        yourEntriesWithPRs @ opponentEntries

    // Group by event
    let grouped =
        fullEntries
        |> List.groupBy (fun (_, event, _) -> event)

    // Score each event
    let scoreEvent (entries: (Identifier * Identifier * Time) list) =
        let sorted =
            entries
            |> List.sortBy (fun (_, _, t) ->
                match t with
                | Float f -> f
                | MinuteTime (m, s) -> m * 60.0 + s
            )

        sorted
        |> List.mapi (fun i (athlete, _, _) ->
            if i < List.length meet.Scoring then
                let pts = meet.Scoring.[i].Score
                if Set.contains athlete yourRoster then pts else 0
            else 0
        )
        |> List.sum

    grouped
    |> List.map (fun (_, entries) -> scoreEvent entries)
    |> List.sum


let getOpposingAthletes (meet: MeetDeclaration) (yourTeam: Identifier) (state: EvalState) : AthleteDeclaration list =
    meet.Teams
    |> List.filter (fun t -> t <> yourTeam)
    |> List.collect (fun rosterName ->
        match Map.tryFind rosterName state.Rosters with
        | Some roster ->
            roster
            |> Set.toList
            |> List.choose (fun name -> Map.tryFind name state.Athletes)
        | None -> []
    )

// Generates all possible (athlete, event) assignment list
let generateAssignments (athletes: AthleteDeclaration list) (events: Identifier list) : Assignment list =
    let possibleEntries =
        athletes
        |> List.collect (fun a ->
            a.Events
            |> List.filter (fun e -> List.contains e events)
            |> List.map (fun e -> (a.Name, e)))

    let rec powerset = function
        | [] -> [ [] ]
        | x::xs ->
            let rest = powerset xs
            rest @ (rest |> List.map (fun r -> x :: r))

    powerset possibleEntries

// Main function for running the optimization
let runOptimization (state: EvalState) (o: Optimize) : EvalState =
    match Map.tryFind o.Meet state.Meets, Map.tryFind o.Team state.Rosters with
    | Some meet, Some roster ->
        let athletes =
            roster
            |> Set.toList
            |> List.choose (fun name -> Map.tryFind name state.Athletes)

        let opponents = getOpposingAthletes meet o.Team state

        printfn "Optimizing %s for %s" o.Team o.Meet
        printfn "Events: %A" meet.Events
        printfn "Scoring: %A" meet.Scoring
        printfn "Your Athletes: %A" (athletes |> List.map (fun a -> a.Name))
        printfn "Opponents: %A" (opponents |> List.map (fun a -> a.Name))

        let assignments = generateAssignments athletes meet.Events
        printfn "Generated %d assignments" (List.length assignments)
        //printfn "%A" assignments
        let yourRoster = roster
        let bestAssignment, bestScore =
            assignments
            |> List.map (fun a ->
                let score = scoreAssignment a state meet yourRoster opponents
                (a, score)
            )
            |> List.maxBy snd

        printfn "Best score: %d" bestScore
        printfn "Best assignment: %A" bestAssignment


        state

    | Some _, None ->
        printfn "Roster %s not found" o.Team
        state

    | None, _ ->
        printfn "Meet %s not found" o.Meet
        state



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

let eval (prog: Program) =
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
            | Optimize o -> runOptimization state o, lastPdf

        ) (emptyState, None) prog
    0
