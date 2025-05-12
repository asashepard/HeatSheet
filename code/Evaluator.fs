module Evaluator

open AST
open LatexCreator

// ----------------------  Environment ----------------------

type EvalState = {
    // All the declared athlete variables
    Athletes: Map<Identifier, AthleteDeclaration>

    // All the declared rosters. Maps an identifer to a set of athletes.
    // uses a set here to improve efficiency later; also avoids duplicates (which I check for explicitly anyway)
    Rosters: Map<Identifier, Set<Identifier>> 

    // All the declared meets. 
    Meets: Map<Identifier, MeetDeclaration>
}

let emptyState = {
    Athletes = Map.empty
    Rosters = Map.empty
    Meets = Map.empty
}

// -----------------------------  Helpers -----------------------------------------

// roster not found
let RNF r= failwith $"Error: Roster {r} not found."

// athlete not found
let ANF a= failwith $"Error: Athlete {a} not found."

// meet not found
let MNF m= failwith $"Error: Meet {m} not found."

// ----------------------  Variable Declarations -----------------------------------

/// Creates an athlete variable. Does not allow duplicates.
let declareAthlete (state: EvalState) (a: AthleteDeclaration) =
    if Map.containsKey a.Name state.Athletes then failwith $"Error: Athlete {a.Name} already defined. Please use a different identifer."
    else { state with Athletes = state.Athletes.Add(a.Name, a) }

/// Creates a roster variable. Does not allow duplicates.
let declareRoster (state: EvalState) (r: RosterDeclaration) =
    if Map.containsKey r.Name state.Rosters then failwith $"Error: Roster {r.Name} already defined. Please use a different identifer."
    else { state with Rosters = state.Rosters.Add(r.Name, Set.ofList r.Athletes) }

/// Creates a meet variable
let declareMeet (state: EvalState)(m: MeetDeclaration) =
    if Map.containsKey m.Name state.Meets then failwith $"Error: Meet {m.Name} already defined. Please use a different identifer."
    else {state with Meets = state.Meets.Add(m.Name, m)}

// ----------------------  Variable Mutations / Redeclarations ----------------------

/// Updates an athlete to something else.
let updateAthlete (state: EvalState) (a: AthleteUpdate) =
    if Map.containsKey a.UpdateName state.Athletes then { state with Athletes = state.Athletes.Add(a.UpdateName, {Name = a.UpdateName; Events = a.NewEvents; PRs = a.NewPRs}) }
    else ANF a.UpdateName

/// Adds an athlete to a roster
let addToRoster (state: EvalState) (ra: RosterAdd) =
    match Map.tryFind ra.AthleteToAdd state.Athletes, Map.tryFind ra.Roster state.Rosters with
    | Some _, Some members ->
        if Set.contains ra.AthleteToAdd members then failwith $"Error: Athlete {ra.AthleteToAdd} is already in roster {ra.Roster}."
        else { state with Rosters = state.Rosters.Add(ra.Roster, members.Add ra.AthleteToAdd) }
    | Some _, None ->   RNF ra.Roster
    | None, Some _ ->   ANF ra.AthleteToAdd
    | None, None ->     failwith $"Error: Athlete {ra.AthleteToAdd} and roster {ra.Roster} not found."

/// Remove from roster
let removeFromRoster (state: EvalState) (rm: RosterRemoval) =
    match Map.tryFind rm.AthleteToRemove state.Athletes, Map.tryFind rm.Roster state.Rosters with
    | Some _, Some members ->
        if Set.contains rm.AthleteToRemove members then 
            let newSet = Set.filter (fun x -> x <> rm.AthleteToRemove) members
            { state with Rosters = state.Rosters.Add(rm.Roster, newSet) }
        else failwith $"Error: Athlete {rm.AthleteToRemove} not found in roster {rm.Roster}."
    | Some _, None ->   RNF rm.Roster
    | None, Some _ ->   failwith $"Error: Athlete {rm.AthleteToRemove} not found."
    | None, None ->     failwith $"Error: Athlete {rm.AthleteToRemove} and roster {rm.Roster} not found."

/// Adds a roster to a meet
let addToMeet (state: EvalState) (ma: MeetAdd) =
    match Map.tryFind ma.Meet state.Meets, Map.tryFind ma.TeamToAdd state.Rosters with
    | Some meet, Some _ ->
        if List.contains ma.TeamToAdd meet.Teams then failwith $"Error: Roster {ma.TeamToAdd} already in meet {ma.Meet}."
        else
            let updatedMeet = { meet with Teams = ma.TeamToAdd :: meet.Teams }
            { state with Meets = state.Meets.Add(ma.Meet, updatedMeet) }
    | Some _, None -> RNF ma.TeamToAdd
    | None, Some _ -> MNF ma.Meet
    | None, None -> failwith $"Error: Meet {ma.Meet} and roster {ma.TeamToAdd} not found."

/// Updates and or adds a PR for a specific event for an athlete
let changePR (state: EvalState) (pc: SetPR) =
    match Map.tryFind pc.Name state.Athletes with
    | Some athlete ->
        let updatedPRs =
            pc.NewPR :: List.filter (fun pr -> pr.Event <> pc.NewPR.Event) athlete.PRs
        let updatedEvents =
            if List.contains pc.NewPR.Event athlete.Events then athlete.Events
            else pc.NewPR.Event :: athlete.Events
        let updatedAthlete = { athlete with Events = updatedEvents; PRs = updatedPRs }
        printfn "%A" updatedAthlete
        { state with Athletes = state.Athletes.Add(pc.Name, updatedAthlete) }
    | None -> ANF pc.Name
    
// ---------------------- Optimizer -----------------------------------
 
type Assignment = (Identifier * Identifier) list

// Expands the roster's athletes personal records into a list
let expandPRs (assignment: Assignment) (state: EvalState) : (Identifier * Identifier * Time) list =
    assignment
    |> List.choose (fun (athlete, event) ->
        match Map.tryFind athlete state.Athletes with
        | Some a ->
            match List.tryFind (fun pr -> pr.Event = event) a.PRs with 
            | Some pr -> Some (athlete, event, pr.Time)
            | None -> None
        | None -> None
    )

// Gets a list of all of the opponents' personal records
let getOpponentPRs opponents=
    opponents |> List.collect (fun a -> a.PRs |> List.map (fun pr -> (a.Name, pr.Event, pr.Time)))

// Converts times into seconds for scoring purposes
let scoreTime = function
    | Float f -> f
    | MinuteTime (m, s) -> m * 60.0 + s


let scoreEvent (entries: (Identifier * Identifier * Time) list) (meet: MeetDeclaration) (yourRoster: Set<Identifier>) : int =
    entries
    |> List.sortBy (fun (_, _, t) -> scoreTime t)
    |> List.mapi (fun i (athlete, _, _) ->
        if i < List.length meet.Scoring then
            let pts = meet.Scoring.[i].Score
            if Set.contains athlete yourRoster then pts else 0
        else 0
    )
    |> List.sum

let scoreAssignment (assignment: Assignment) (state: EvalState) (meet: MeetDeclaration) (yourRoster: Set<Identifier>) (opponents: AthleteDeclaration list) : int =
    let allEntries =
        let yourEntries = expandPRs assignment state
        let opponentEntries = getOpponentPRs opponents
        yourEntries @ opponentEntries

    allEntries
    |> List.groupBy (fun (_, event, _) -> event)
    |> List.sumBy (fun (_, entries) -> scoreEvent entries meet yourRoster)

// Gets a list of all opposing athletes
let getOpposingAthletes (meet: MeetDeclaration) (yourTeam: Identifier) (state: EvalState) : AthleteDeclaration list =
    meet.Teams
    |> List.filter ((<>) yourTeam)
    |> List.collect (fun rosterName ->
        state.Rosters |> Map.tryFind rosterName |> function
        | Some roster -> roster |> Set.toList |> List.choose (fun name -> Map.tryFind name state.Athletes)
        | None -> []
    )

// Generates all possible assignments for athlete, event combinations
let generateAssignments (athletes: AthleteDeclaration list) (events: Identifier list) : Assignment list =
    let possibleEntries =
        athletes
        |> List.collect (fun a ->
            a.Events |> List.filter (fun e -> List.contains e events) |> List.map (fun e -> (a.Name, e)))
    let powerset xs =
        let folder acc x = List.fold (fun acc' subset -> (x :: subset) :: acc') acc acc
        List.fold folder [ [] ] xs
    powerset possibleEntries

// Computes the optimal athlete to event assignments. 
// Assumptions: 
//      1. All opposing athletes run every event that they have a PR in
//      2. Our athletes can run as many events as they want, as long as they satisfy meet requirements
//      3. Athletes always run their PRs
let runOptimization (state: EvalState) (meet: MeetDeclaration) (roster: Set<Identifier>) (team: Identifier) : EvalState =
    let athletes = roster |> Set.toList |> List.choose (fun name -> Map.tryFind name state.Athletes)
    let opponents = getOpposingAthletes meet team state
    let assignments = 
        let allPossibleAssignments = generateAssignments athletes meet.Events
        match meet.MaxAthletesPerEvent with
        | Some max -> allPossibleAssignments |> List.filter (fun assignment ->
                assignment |> List.countBy fst |> List.forall (fun (_, count) -> count <= max)
            )
        | None -> allPossibleAssignments
    let bestAssignment, bestScore =
        assignments
        |> List.map (fun a -> a, scoreAssignment a state meet roster opponents)
        |> List.maxBy snd
    
    // for now, we just print out to the console
    printfn "Best score: %d" bestScore
    printfn "Best assignment: %A" bestAssignment
    state

// Runs the optimization statment
let optimize (state: EvalState) (o: Optimize) : EvalState =
    match Map.tryFind o.Meet state.Meets, Map.tryFind o.Team state.Rosters with
    | Some meet, Some roster -> runOptimization state meet roster o.Team
    | Some _, None ->   RNF o.Team
    | None, Some _ ->   failwith $"Error: Meet {o.Meet} not found."
    | None, None ->     failwith $"Error: Meet {o.Meet} and roster {o.Team} not found."


// ------------------ LaTeX Formatting Helpers --------------------

/// Sorts athlete events in the following priority:
/// 1. Current athlete events that are numeric, sorted ascending
/// 2. Current athlete events that are text, like "steeplechase", sorted alphabetically
/// 3. Events the athlete has a PR in but does not currently run, numeric only, sorted ascending
/// 4. Events the athlete has a PR in but does not currently run, text only, sorted alphabetically
let sortEvents (events: string list) (athlete: AthleteDeclaration) : string list =
    // uses regex to parse numeric part out of an event e.g. "100m" -> 100
    let parseNumeric e =
        match System.Text.RegularExpressions.Regex.Match(e, @"^\d+") with
        | m when m.Success -> Some (int m.Value)
        | _ -> None

    events |> List.sortBy 
        (fun e ->
            if List.contains e athlete.Events then // current event for athlete
                match parseNumeric e with
                | Some n -> Choice1Of4 n // Event is a numeric event, sort ascending
                | None -> Choice2Of4 e // Event starts with a letter, sort alphabetically
            else
                match parseNumeric e with
                | Some n -> Choice3Of4 n
                | None -> Choice4Of4 e
        )

/// Gets all events an athlete can run or has a PR for
let allEvents (athlete: AthleteDeclaration) (prEvents: string list) =
    List.append athlete.Events prEvents |> List.distinct |> fun evs -> sortEvents evs athlete

/// Gets the right suffix for the place. Only works up to 99
let suffix place =
    if place % 100 >= 11 && place % 100 <= 13 then "th"
    else
    match place % 10 with
    | 1 -> "st"
    | 2 -> "nd"
    | 3 -> "rd"
    | _ -> "th"

// ----------------------- LaTeX Formatting for RosterShow --------------------------

/// Sets up the packages for the LaTeX document header and begins the document
let latexHeaderRoster = 
    String.concat "\n" [
        "\\documentclass[11pt]{article}"
        "\\usepackage[margin=1in]{geometry}"
        "\\usepackage{booktabs}"
        "\\usepackage{longtable}"
        "\\usepackage{tabularx}"
        "\\usepackage{siunitx}"
        "\\usepackage[table]{xcolor}"
        "\\usepackage{titlesec}"
        "\\usepackage{helvet}"
        "\\renewcommand{\\familydefault}{\\sfdefault}"
        "\\titleformat{\\section}{\\Large\\bfseries\\centering}{}{0em}{}"
        "\\definecolor{rowgray}{gray}{0.95}"
        "\\begin{document}"
    ]

/// Formats the time for the LaTeX document
let formatTime = function
    | Float f -> sprintf "%.2f" f
    | MinuteTime (m, s) -> sprintf "%.0f:%.2f" m s


/// Formats the entire athlete cell, including all events and PRs if available, and returns a tuple of each cell
let formatAthleteCells (a: AthleteDeclaration) : string * string * string =
    let events = String.concat ", " a.Events
    let prs =
        a.PRs
        |> List.map (fun pr -> sprintf "%s: %s" pr.Event (formatTime pr.Time))
        |> String.concat ", "
    a.Name, events, prs

/// Creates all the rows for the athlete, with each one having an event
let renderAthleteRows (state: EvalState) (name: Identifier) : string list =
    match Map.tryFind name state.Athletes with
    | None -> ANF name
    | Some a ->
        let prEvents = a.PRs |> List.map (fun pr -> pr.Event)
        let combinedEvents = allEvents a prEvents
        let rows =
            combinedEvents 
            |> List.map 
                (fun event ->
                    // Shows how to distinguish between current events and events that were only given a PR
                    let displayEvent =
                        if List.contains event a.Events then event
                        else sprintf "\\textit{%s}" event 

                    // The PR for the event
                    let pr = 
                        match List.tryFind (fun pr -> pr.Event = event) a.PRs with
                        | Some pr -> formatTime pr.Time
                        | None -> ""

                    displayEvent, pr
                )
        
        // Returns a list of all the row strings in good LaTeX formatting
        match rows with
        | [] -> [ $"{a.Name} & & \\\\" ] 
        | (ev, pr) ::rest ->
            let firstRow = $"{a.Name} & {ev} & {pr} \\\\" // appends the name of the first athlete only in the first row
            let restRows = rest |> List.map (fun (e, pr) -> $"& {e} & {pr} \\\\")
            firstRow :: restRows @ [ "\\midrule" ]

// Builds the entire table for the roster
let buildRosterTable (state: EvalState) (athletes: Set<Identifier>) : string =
    let header = "\\toprule\n\\textbf{Name} & \\textbf{Event} & \\textbf{PR} \\\\\n\\midrule"
    let rows = athletes |> Set.toList |> List.map (renderAthleteRows state) |> List.concat

    let rowsFormatted =
        [ "\\rowcolors{2}{gray!10}{white}"; "\\begin{tabularx}{\\textwidth}{lXr}" ; header ]
        |> List.append rows
        |> List.append [ "\\bottomrule"; "\\end{tabularx}" ]
        |> String.concat "\n"
    rowsFormatted

/// Constructs the entire latex document for roster output
let buildRosterLatexDocument (state: EvalState) (roster: string) (athletes: Set<Identifier>): string =
    let table = buildRosterTable state athletes
    String.concat "\n" [
        latexHeaderRoster
        $"\\section*{{Roster: {roster}}}"
        table
        "\\end{document}"
    ]

/// Generates LaTeX document for the roster, based on the string returned from above functions
let generateLatexRosterShow (state: EvalState) (roster: Identifier) : string option =
    match Map.tryFind roster state.Rosters with
    | None -> RNF roster
    | Some athletes ->
        let tex = buildRosterLatexDocument state roster athletes
        Some (runPdfLatex tex "." (roster + "_roster"))


// ----------------------- LaTeX Formatting for MeetShow --------------------------

/// Sets up the packages for the LaTeX document header and begins the document
let latexHeaderMeet =
    String.concat "\n" [
        "\\documentclass[11pt]{article}"
        "\\usepackage[margin=1in]{geometry}"
        "\\usepackage{booktabs}"
        "\\usepackage{tabularx}"
        "\\usepackage{titlesec}"
        "\\usepackage{helvet}"
        "\\renewcommand{\\familydefault}{\\sfdefault}"
        "\\titleformat{\\section}{\\Large\\bfseries\\centering}{}{0em}{}"
        "\\begin{document}"
    ]

/// Renders the header information for the meet
let meetDocumentHeader meet =
    let eventStringList = String.concat ", " meet.Events
    let scoringStringList =
        meet.Scoring
        |> List.map (fun s -> $"{s.Place}{suffix s.Place}: {s.Score}")
        |> String.concat ", "
    [
        $"\\section*{{Meet: {meet.Name}}}"
        $"Events: {eventStringList}\\\\"
        $"Scoring: {scoringStringList}\\\\"
    ]

/// gets a string list of teams separated by commas
let teamList meet = "\\subsection*{Teams}\n" + String.concat ", " meet.Teams

/// Builds the event table with potential athletes and their PRs
let eventTable (state: EvalState) (meet: MeetDeclaration) (event: string) : string =
    let allAthletes =
        meet.Teams
        |> List.collect (fun team ->
            match Map.tryFind team state.Rosters with
            | Some roster ->
                roster
                |> Set.toList
                |> List.choose (fun name -> Map.tryFind name state.Athletes)
                |> List.choose (fun a ->
                    a.PRs
                    |> List.tryFind (fun pr -> pr.Event = event)
                    |> Option.map (fun pr -> a.Name, team, pr.Time)
                )
            | None -> []
        )
        |> List.sortBy (fun (_, _, time) ->
            match time with
            | Float f -> f
            | MinuteTime (m, s) -> m * 60.0 + s
        )

    let rows =
        allAthletes
        |> List.mapi (fun i (name, team, time) ->
            let place = $"{i + 1}{suffix (i + 1)}"
            $"{place} & {name} ({team}) & {formatTime time} \\\\"
        )

    String.concat "\n" (
        [ $"\\subsection*{{Event: {event}}}"
          "\\begin{tabularx}{\\linewidth}{lXr}"
          "\\toprule"
          "Place & All Potential Athletes & Time \\\\"
          "\\midrule" ]
        @ rows @
        [ "\\bottomrule"
          "\\end{tabularx}" ]
    )

/// Constructs the full LaTeX document string for a meet
let buildMeetLatexDocument (state: EvalState) (meet: MeetDeclaration) : string =
    let header = latexHeaderMeet
    let docHeader = meetDocumentHeader meet
    let teams = teamList meet
    let eventTables = meet.Events |> List.map (eventTable state meet)
    
    String.concat "\n\n" (
        [header]
        @ docHeader
        @ [teams]
        @ eventTables
        @ ["\\end{document}"]
    )

/// Generates the meet LaTeX file using the current state and a meet ID
let generateLatexMeetShow (state: EvalState) (meetId: Identifier) : string option =
    match Map.tryFind meetId state.Meets with
    | None -> MNF meetId
    | Some meet ->
        let tex = buildMeetLatexDocument state meet
        Some (runPdfLatex tex "." (meetId + "_meet"))



// --------------------  Evaluation Helpers ----------------------

/// Main function for the roster show call
/// TODO: incorporate a path parameter
let rosterShow state rs =
    match generateLatexRosterShow state rs.RosterToShowName with
    | Some path ->
        // printfn "PDF generated at: %s" path
        state, Some path
    | None -> RNF rs.RosterToShowName

/// Main function for the meet show call
/// TODO: incorporate a path parameter
let meetShow state ms=
    match generateLatexMeetShow state ms.MeetToShowName with
    | Some path ->
        // printfn "PDF generated at: %s" path
        state, Some path
    | None -> MNF ms.MeetToShowName

// ----------------------  Evaluation ----------------------

let eval (prog: Program) =
    let finalState, _ =
        List.fold (fun (state, lastPdf) stmt ->
            match stmt with
            | Athlete a->      declareAthlete state a, lastPdf
            | AthleteUpdate a ->    updateAthlete state a, lastPdf
            | PRChange pc ->                changePR state pc, lastPdf
            | Roster r->        declareRoster state r, lastPdf
            | RosterAdd ra->            addToRoster state ra, lastPdf
            | RosterRemoval rm ->   removeFromRoster state rm, lastPdf
            | RosterShow rs->        rosterShow state rs
            | MeetShow ms ->           meetShow state ms
            | Meet m ->           declareMeet state m, lastPdf
            | MeetAdd ma ->               addToMeet state ma, lastPdf
            | Optimize o ->              optimize state o, lastPdf
        ) (emptyState, None) prog
    0
