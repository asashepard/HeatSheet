module Evaluator

open AST
open LatexCreator
open System
open System.Collections.Generic
open System.Threading.Tasks
open System.Collections.Concurrent
open System.Threading

// --------------------------------  Environment ------------------------------

type EvalState = {
    // All the declared athlete variables
    Athletes: Map<Identifier, AthleteDeclaration>

    // All the declared rosters. Maps an identifer to a set of athletes.
    // uses a set here to improve efficiency later; also avoids duplicates (which I check for explicitly anyway)
    Rosters: Map<Identifier, Set<Identifier>> 

    // All the declared meets. 
    Meets: Map<Identifier, MeetDeclaration>

    // The optimization type.
    OptimizationMethod: OptimizationType
}

type Assignment = (Identifier * Identifier) list
type Hist = ConcurrentDictionary<int,int>
type Optimization = {
    meet: MeetDeclaration
    team: Identifier
    assignment: Assignment
    expected: float
    placement: Map<int, float>
    expectedAll: Map<Identifier, float>
    histograms: Map<Identifier, Hist>
}

let emptyState = {
    Athletes = Map.empty
    Rosters = Map.empty
    Meets = Map.empty
    OptimizationMethod = Basic
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

/// Sets the method to optimize with
let optimizationType state so= 
    match so with
    | Basic -> {state with OptimizationMethod = Basic}
    | Simulation -> {state with OptimizationMethod = Simulation}

// ----------------------  Variable Mutations / Redeclarations ----------------------

/// Updates an athlete to something else.
let updateAthlete (state: EvalState) (a: AthleteUpdate) =
    if Map.containsKey a.UpdateName state.Athletes then { state with Athletes = state.Athletes.Add(a.UpdateName, {Name = a.UpdateName; Events = a.NewEvents; PRs = a.NewPRs; MaxEvents = a.NewMaxEvents}) }
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

/// Removes a roster from a meet
let removeFromMeet (state: EvalState) (mr: MeetRemoval) =
    match Map.tryFind mr.Meet state.Meets, Map.tryFind mr.TeamToRemove state.Rosters with
    | Some meet, Some roster ->
        if List.contains mr.TeamToRemove meet.Teams then 
            let updatedMeet = { meet with Teams = List.filter ((<>) mr.TeamToRemove) meet.Teams }
            { state with Meets = state.Meets.Add(mr.Meet, updatedMeet) }
        else failwith $"Error: Roster {mr.TeamToRemove} not in meet {mr.Meet}."

    | Some _, None -> RNF mr.TeamToRemove
    | None, Some _ -> MNF mr.Meet
    | None, None -> failwith $"Error: Meet {mr.Meet} and roster {mr.TeamToRemove} not found."


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
        { state with Athletes = state.Athletes.Add(pc.Name, updatedAthlete) }
    | None -> ANF pc.Name

/// duplicates the athlete, roster, or meet
let duplicate state d = 
    match d with
    | DuplicateAthlete da -> 
        match Map.tryFind da.AthleteToDuplicate state.Athletes with
        | Some athlete -> 
            let copied = { athlete with Name = da.NewIdentifier }
            { state with Athletes = state.Athletes.Add(da.NewIdentifier, copied) }
        | None -> ANF da.AthleteToDuplicate
    | DuplicateRoster dr -> 
        match Map.tryFind dr.RosterToDuplicate state.Rosters with
        | Some roster -> 
            { state with Rosters = state.Rosters.Add(dr.RosterToDuplicate, roster) }
        | None -> RNF dr.RosterToDuplicate
    | DuplicateMeet dm -> 
        match Map.tryFind dm.MeetToDuplicate state.Meets with
        | Some meet -> 
            let copied = { meet with Name = dm.NewIdentifier }
            { state with Meets = state.Meets.Add(dm.NewIdentifier, copied) }
        | None -> MNF dm.MeetToDuplicate


// --------------------------------------- LaTeX Formatting Helpers ------------------------------------

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

// ------------------------------------ LaTeX Formatting for RosterShow ----------------------------------

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
    | HourMinuteTime (h, m, s) -> sprintf "%.0f:%.0f:%.2f" h m s

/// Formats the entire athlete cell, including all events and PRs if available, and returns a tuple of each cell
let formatAthleteCells (a: AthleteDeclaration) : string * string * string =
    let events = String.concat ", " a.Events
    let prs =
        a.PRs
        |> List.map (fun pr -> sprintf "%s: %s" pr.Event (formatTime pr.Time))
        |> String.concat ", "
    a.Name, events, prs

/// Formats a single event row (with italics if only a PR exists)
let formatEventRow (a: AthleteDeclaration) (event: string) : string * string =
    let displayEvent =
        if List.contains event a.Events then event
        else $"\\textit{{{event}}}"

    let pr =
        match List.tryFind (fun pr -> pr.Event = event) a.PRs with
        | Some pr -> formatTime pr.Time
        | None -> ""

    displayEvent, pr

/// Creates all the rows for the athlete, with each one having an event
let renderAthleteRows (state: EvalState) (name: Identifier) : string list =
    match Map.tryFind name state.Athletes with
    | None -> ANF name
    | Some a ->
        let prEvents = a.PRs |> List.map (fun pr -> pr.Event)
        let combinedEvents = allEvents a prEvents
        let rows = combinedEvents |> List.map (formatEventRow a)

        match rows with
        | [] -> [ $"{a.Name} & & \\\\" ]
        | (ev, pr) :: rest ->
            let firstRow = $"{a.Name} & {ev} & {pr} \\\\"
            let restRows = rest |> List.map (fun (e, pr) -> $"& {e} & {pr} \\\\")
            firstRow :: restRows @ [ "\\midrule" ]

// Builds the entire table for the roster
let buildRosterTable (state: EvalState) (athletes: Set<Identifier>) : string =
    let headerLines = [
        "\\rowcolors{2}{gray!10}{white}"
        "\\begin{tabularx}{\\textwidth}{lXr}"
        "\\toprule"
        "\\textbf{Name} & \\textbf{Event} & \\textbf{PR} \\\\"
    ]

    let rows = athletes |> Set.toList |> List.map (renderAthleteRows state) |> List.concat

    let tableBody =
        match rows with
        | [] -> []
        | _ -> ("\\midrule" :: rows) @ ["\\bottomrule"]

    String.concat "\n" (headerLines @ tableBody @ [ "\\end{tabularx}" ])

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
let generateLatexRosterShow (state: EvalState) (roster: Identifier) (path: string option) : string option =
    match Map.tryFind roster state.Rosters with
    | None -> RNF roster
    | Some athletes ->
        let tex = buildRosterLatexDocument state roster athletes
        let filename = defaultArg path (roster + "_roster")
        Some (runPdfLatex tex "." filename)

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
            | HourMinuteTime(h,m,s) -> h * 60.0 * 60.0 + m * 60.0 + s
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
    let eventTables = meet.Events |> List.map (eventTable state meet)
    
    String.concat "\n\n" (
        [header]
        @ docHeader
        @ eventTables
        @ ["\\end{document}"]
    )

/// Generates the meet LaTeX file using the current state and a meet ID
let generateLatexMeetShow (state: EvalState) (meetId: Identifier) (path: string option) : string option =
    match Map.tryFind meetId state.Meets with
    | None -> MNF meetId
    | Some meet ->
        let tex = buildMeetLatexDocument state meet
        let filename = defaultArg path (meetId + "_meet")
        Some (runPdfLatex tex "." filename)

// ----------------------- Optimizer --------------------------

let cvLookup (event : Identifier) =
    match event with
    | e when e.StartsWith "100"   -> 0.010
    | e when e.StartsWith "200"   -> 0.010
    | e when e.StartsWith "400"   -> 0.010
    | e when e.StartsWith "800"   -> 0.010
    | e when e.StartsWith "1500"  -> 0.010
    | e when e.StartsWith "3000"  -> 0.014
    | e when e.StartsWith "5000"  -> 0.014
    | e when e.StartsWith "10000" -> 0.014
    | _ -> 0.012

// convert PR time (seconds) to mean of log‑normal (about 1.5 % slower than PR)
let seasonMean secs = secs * 1.015

let rnd = Random()

/// sample a performance time (seconds) given event id and PR (seconds)
let sampleTime (event : Identifier) (prSecs : float) : float =
    let cv   = cvLookup event               // e.g. 0.010
    let sigma = log (1.0 + cv)              // std of log‑space normal
    let mu    = log (seasonMean prSecs)     // mean of log‑space normal
    // Box‑Muller transform for N(0,1)
    let u1 = rnd.NextDouble()
    let u2 = rnd.NextDouble()
    let z  = sqrt(-2.0 * log u1) * cos (2.0 * Math.PI * u2)
    exp (mu + sigma * z)

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
let getOpponentPRs
    (state: EvalState)
    (meet: MeetDeclaration)
    (opponents: AthleteDeclaration list)
    : (Identifier * Identifier * Time) list =

    // how many each team can enter in this meet
    let maxPerEvent = defaultArg meet.MaxAthletesPerEvent System.Int32.MaxValue

    // helper to sort times
    let toSecs = function
        | Float f               -> f
        | MinuteTime(m,s)       -> m*60.0 + s
        | HourMinuteTime(h,m,s) -> float h*(float)3600 + float m*(float)60 + s

    opponents
    // build (team,name,event,time) tuples
    |> List.collect (fun a ->
        a.PRs
        |> List.filter (fun pr -> List.contains pr.Event meet.Events)
        |> List.map (fun pr ->
            // find which roster this athlete belongs to
            let teamName =
                state.Rosters
                |> Seq.find (fun (KeyValue(t,members)) -> members.Contains a.Name)
                |> fun (KeyValue(t,_)) -> t

            (teamName, a.Name, pr.Event, pr.Time)
            )
        )
    // group by event
    |> List.groupBy (fun (_,_,ev,_) -> ev)
    |> List.collect (fun (_ev, evEntries) ->
        // within each event, group by team
        evEntries
        |> List.groupBy (fun (team,_,_,_) -> team)
        |> List.collect (fun (_team, teamEntries) ->
            teamEntries
            // take the fastest `maxPerEvent` for this team/event
            |> List.sortBy    (fun (_,_,_,t) -> toSecs t)
            |> List.truncate  maxPerEvent
            // drop the team tag again
            |> List.map        (fun (_, name, ev, time) -> (name, ev, time))
            )
        )

// Converts times into seconds for scoring purposes
let scoreTime = function
    | Float f -> f
    | MinuteTime (m, s) -> m * 60.0 + s
    | HourMinuteTime(h,m,s) -> h * 60.0 * 60.0 + m * 60.0 + s

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

let scoreEventStochastic
        (entries     : (Identifier * Identifier * Time) list)
        (meet        : MeetDeclaration)
        (yourRoster  : Set<Identifier>) : int =

    // convert every athlete’s PR to a simulated mark
    let sampled =
        entries
        |> List.map (fun (name, event, t) ->
            let seconds = scoreTime t
            let simMark = sampleTime event seconds
            (name, event, simMark))

    // rank lower time = better place
    sampled
    |> List.sortBy (fun (_,_,sec) -> sec)
    |> List.mapi (fun i (ath,_,_) ->
        if i < List.length meet.Scoring then
            let pts = meet.Scoring.[i].Score
            if Set.contains ath yourRoster then pts else 0
        else 0)
    |> List.sum

let simulateMeetOnce
        (yourTeam       : Identifier)
        (yourAssignment : Assignment)
        (state          : EvalState)
        (meet           : MeetDeclaration)
        (yourRoster     : Set<Identifier>)
        (opponents      : AthleteDeclaration list) : Map<Identifier,int> =

    // build once (deterministic structures)
    let yourEntries      = expandPRs yourAssignment state
    let opponentEntries  = getOpponentPRs state meet opponents
    let allEntries       = yourEntries @ opponentEntries      // (ath, event, PR)
    let groupedByEvent   = allEntries |> List.groupBy (fun (_,e,_) -> e)

    // dictionary team → points
    let totals = Dictionary<Identifier,int>()
    totals.[yourTeam] <- 0

    // helper to add points
    let inline addPts team pts =
        let cur = if totals.ContainsKey team then totals.[team] else 0
        totals.[team] <- cur + pts

    // simulate every event
    for (_,entries) in groupedByEvent do
        // sample marks
        let sampled =
            entries
            |> List.map (fun (ath,event,t) ->
                let sec = scoreTime t
                let mark = sampleTime event sec
                (ath,event,mark))

        // rank
        sampled
        |> List.sortBy (fun (_,_,sec) -> sec)
        |> List.mapi (fun place (ath,_,_) ->
            if place < List.length meet.Scoring then
                let pts = meet.Scoring.[place].Score
                // which team is this athlete on?
                let team =
                    if Set.contains ath yourRoster then yourTeam  // you may store it
                    else                                           // look up in roster map
                        state.Rosters
                        |> Seq.pick (fun kv ->
                            let (teamName, members) = kv.Key, kv.Value
                            if members.Contains ath then Some teamName else None)
                addPts team pts)
        |> ignore

    // convert to immutable Map
    totals |> Seq.map (|KeyValue|) |> Map.ofSeq

/// Gets the total team score for a certain assignment
let scoreAssignment (assignment: Assignment) (state: EvalState) (meet: MeetDeclaration) (yourRoster: Set<Identifier>) (opponents: AthleteDeclaration list) : int =
    let allEntries =
        let yourEntries = expandPRs assignment state
        let opponentEntries = getOpponentPRs state meet opponents
        yourEntries @ opponentEntries

    allEntries
    |> List.groupBy (fun (_, event, _) -> event)
    |> List.sumBy (fun (_, entries) -> scoreEvent entries meet yourRoster)

let inline bump (h : Hist) pts =
    let cnt = if h.ContainsKey pts then h.[pts] else 0
    h.[pts] <- cnt + 1

type PlacementStats =
  { Expected       : Map<Identifier,float>      // team → mean points
    Histograms     : Map<Identifier,Hist>       // team → histogram
    PlacementProb  : Map<int,float> }           // your team, place → P  // place → probability

let placementStats
        (trials      : int)
        (assignment  : Assignment)
        (state       : EvalState)
        (meet        : MeetDeclaration)
        (yourRoster  : Set<Identifier>)
        (opponents   : AthleteDeclaration list)
        (yourTeam    : Identifier)
        (updateProgress : unit -> unit) : PlacementStats =

    // team  ->  histogram(points -> count)
    let teamHists = ConcurrentDictionary<Identifier,Hist>()

    // ensure that a histogram exists for a team
    let ensure team =
        if not (teamHists.ContainsKey team) then teamHists.[team] <- Hist()

    // bump a (mutable) histogram
    let bump (hist:Hist) pts =
        let cur = if hist.ContainsKey pts then hist.[pts] else 0
        hist.[pts] <- cur + 1

    // placement histogram for *your* team only
    let placeCounts = ConcurrentDictionary<int,int>()

    Parallel.For(1, trials + 1, fun trial ->
        // simulate an entire meet -> team → points map
        let teamPoints =
            simulateMeetOnce yourTeam assignment state meet yourRoster opponents

        // update every team’s histogram
        for KeyValue(team,pts) in teamPoints do
            ensure team
            bump teamHists.[team] pts

        // rank teams by points (ties keep stable order)
        let sorted =
            teamPoints
            |> Map.toList
            |> List.sortByDescending snd

        match List.tryFindIndex (fun (t,_) -> t = yourTeam) sorted with
        | Some idx ->
            let place = idx + 1          // 0‑based → 1‑based
            bump placeCounts place
        | None ->
            // your team scored 0 and wasn't present – treat as last place
            let last = List.length sorted + 1
            bump placeCounts last

        updateProgress()
    ) |> ignore

    // convert mutable histograms to immutable
    let expected =
        teamHists
        |> Seq.map (fun (KeyValue(team,hist)) ->
            let weightedSum =
                hist |> Seq.sumBy (fun (KeyValue(pts,cnt)) ->
                          float pts * float cnt)
            let mean = weightedSum / float trials
            team, mean)
        |> Map.ofSeq

    let histograms =
        teamHists |> Seq.map (|KeyValue|) |> Map.ofSeq

    let placementProb =
        placeCounts
        |> Seq.map (|KeyValue|)
        |> Seq.map (fun (pl,cnt) -> pl, float cnt / float trials)
        |> Map.ofSeq

    { Expected      = expected
      Histograms    = histograms
      PlacementProb = placementProb }

// Gets a list of all opposing athletes
let getOpposingAthletes (meet: MeetDeclaration) (yourTeam: Identifier) (state: EvalState) : AthleteDeclaration list =
    meet.Teams
    |> List.filter ((<>) yourTeam)
    |> List.collect (fun rosterName ->
        state.Rosters |> Map.tryFind rosterName |> function
        | Some roster -> roster |> Set.toList |> List.choose (fun name -> Map.tryFind name state.Athletes)
        | None -> []
    )

let isValidAssignment
    (assign : Assignment)
    (meet   : MeetDeclaration)
    (state  : EvalState)
    : bool =
    // per‐event capacity
    let checkEventCapacity =
        assign
        |> List.countBy snd
        |> List.forall (fun (ev,count) ->
            match meet.MaxAthletesPerEvent with
            | Some m -> count <= m
            | None   -> true)
    // per‐athlete limit
    let checkAthleteLoads =
        assign
        |> List.groupBy fst
        |> List.forall (fun (ath, asgs) ->
            let maxE = defaultArg (Map.find ath state.Athletes).MaxEvents 2
            asgs.Length <= maxE)
    checkEventCapacity && checkAthleteLoads

/// Greedy assignment generator
let generateGreedyAssignment
    (state                : EvalState)
    (athletes             : AthleteDeclaration list)
    (events               : Identifier list)
    (maxAthletesPerEvent  : int option)
    : Assignment list =

    // 1) build all (athlete,event,time) triples
    let scoreSecs = function
      | Float f                 -> f
      | MinuteTime (m,s)        -> m*60.0 + s
      | HourMinuteTime(h,m,s)   -> float h*3600.0 + float m*60.0 + s

    let allPairs =
      athletes
      |> List.collect (fun a ->
           a.PRs
           |> List.filter (fun pr -> List.contains pr.Event events)
           |> List.map    (fun pr -> a.Name, pr.Event, scoreSecs pr.Time)
         )

    let sorted =
      allPairs |> List.sortBy (fun (_,_,t) -> t)

    // 2) prepare mutable counters
    let athleteCount = Dictionary<Identifier,int>()
    let eventCount   = Dictionary<Identifier,int>()
    let assignment   = ResizeArray<Identifier * Identifier>()

    // 3) loop inside the function
    for (ath,ev,_) in sorted do
        // read “previous” counts via TryGetValue
        let mutable prevAth = 0
        ignore (athleteCount.TryGetValue(ath, &prevAth))
        let mutable prevEv  = 0
        ignore (eventCount.   TryGetValue(ev,  &prevEv))

        // what max this athlete can do?
        let athMax =
          defaultArg
            ((Map.find ath state.Athletes).MaxEvents)
            2

        // what max this event can take?
        let evMax =
          defaultArg maxAthletesPerEvent 100

        if prevAth < athMax && prevEv < evMax then
            assignment.Add (ath, ev)
            athleteCount.[ath] <- prevAth + 1
            eventCount.   [ev]  <- prevEv  + 1

    // 4) back out to module‐level indent
    [ assignment |> Seq.toList ]

let hillClimbOptim
    (initial : Assignment)
    (state   : EvalState)
    (meet    : MeetDeclaration)
    (roster  : Set<Identifier>)
    (opps    : AthleteDeclaration list)
    (trials  : int)
    (iters   : int)
  =
  let mutable bestA = initial
  let mutable bestScore = scoreAssignment bestA state meet roster opps

  let rnd = System.Random()
  for i in 1..iters do
    // pick a random assignment index
    let idx = rnd.Next(bestA.Length)
    let (ath,oldEv) = bestA.[idx]
    // choose one of ath's other events at random
    let otherEs = 
      (Map.find ath state.Athletes).Events
      |> List.filter ((<>) oldEv)
    if otherEs<>[] then
      let newEv = otherEs.[ rnd.Next(otherEs.Length) ]
      let candidate = bestA |> List.mapi (fun j ae -> if j=idx then (ath,newEv) else ae)
      // check feasibility (max per‐event & per‐athlete)
      if isValidAssignment candidate meet state then
        let s = scoreAssignment candidate state meet roster opps
        if s>bestScore then
          bestScore <- s
          bestA     <- candidate

  bestA, bestScore

// Computes the optimal athlete to event assignments. 
// Assumptions: 
//      1. All opposing athletes run every event that they have a PR in
//      2. Our athletes can run as many events as they want, as long as they satisfy meet requirements
//      3. Athletes always run their PRs
let runOptimization (state: EvalState) (meet: MeetDeclaration) (roster: Set<Identifier>) (team: Identifier) =
    let athletes = roster |> Set.toList |> List.choose (fun name -> Map.tryFind name state.Athletes)
    let opponents = getOpposingAthletes meet team state
    let trials    = 20000
    let hillIters = 2000

    // 2a) get the ONE greedy assignment
    let initialAssign =
        generateGreedyAssignment 
            state
            athletes
            meet.Events
            meet.MaxAthletesPerEvent
            |> List.exactlyOne

    // sanity‐check
    if not (isValidAssignment initialAssign meet state) then
        failwith "Greedy produced an invalid assignment!"

    // 2b) score it
    let baseScore = 
        scoreAssignment initialAssign state meet roster opponents
        |> float

    // 2c) hill‑climb to try to improve
    let improvedAssign, improvedScore =
        hillClimbOptim 
            initialAssign 
            state 
            meet 
            roster 
            opponents 
            trials 
            hillIters

    // pick the winner
    let finalAssign, finalScore =
        if float improvedScore > baseScore then
            improvedAssign, float improvedScore
        else
            initialAssign, baseScore
    
    let totalTasks = finalAssign.Length * trials
    printfn "Running %d trials..." trials
    let completed = ref 0
    let printedPct = ref -1

    let updateProgress () =
        let soFar = Interlocked.Increment(completed)
        let pct = 100 * soFar / totalTasks
        if pct <> !printedPct then
            lock printedPct (fun () ->
                if pct <> !printedPct then
                    printedPct := pct
                    printf "\rGlobal Progress: %3d%%\n" pct
                    stdout.Flush()
            )

    let bestAssignment, stats =
        [ finalAssign ]
        |> List.map (fun a ->
            let st = placementStats trials a state meet roster opponents team updateProgress
            (a, st))
        |> List.maxBy (fun (_,st) ->
            st.Expected |> Map.tryFind team |> Option.defaultValue 0.0)
    let expPts =
        stats.Expected |> Map.tryFind team |> Option.defaultValue 0.0
    state, {meet = meet; team = team; assignment = finalAssign; expected = expPts; placement = stats.PlacementProb; expectedAll = stats.Expected; histograms = stats.Histograms}

// ----------------------- LaTeX Formatting for Optimizer --------------------------

/// Optimization document header
let optimizationDocHeader meet o =
    let eventString =
        meet.Events
        |> String.concat ", "

    // e.g. "1st: 10, 2nd: 8, …"
    let scoringString =
        meet.Scoring
        |> List.map (fun s -> sprintf "%d%s: %d" s.Place (suffix s.Place) s.Score)
        |> String.concat ", "

    // e.g. "1st: 47 %, 2nd: 32 %, 3rd: 15 %"
    let placementString =
        o.placement
        |> Map.toList
        |> List.sortBy fst
        |> List.map (fun (pl, prob) ->
            sprintf "%d%s: %.0f\\%%" pl (suffix pl) (prob * 100.0))
        |> String.concat ", "

    let expectedLines =
        o.expectedAll
        |> Map.toList
        |> List.sortByDescending snd
        |> List.map (fun (team,mu) ->
            sprintf "\\quad %s: %.1f\\\\" team mu)

    [ sprintf "\\section*{%s Optimization for Meet: %s}"             o.team meet.Name
      sprintf "\\textbf{Events}: %s\\\\"                             eventString
      sprintf "\\noindent\\textbf{Scoring}: %s\\\\"                  scoringString
      sprintf "\\noindent\\textbf{Placement Probabilities for %s}: %s\\\\" o.team placementString
      sprintf "\\noindent\\textbf{Expected Team Scores}: \\\\" ]
    @ expectedLines

/// Builds the event table for each of the optimized events
let optimizedEventTable (state: EvalState) (opt: Optimization) (event: string) : string =
    let yourRoster = 
        match Map.tryFind opt.team state.Rosters with
        | Some r -> r
        | None -> Set.empty

    let opponentEntries = getOpponentPRs state opt.meet (getOpposingAthletes opt.meet opt.team state)
    let yourEntries = expandPRs opt.assignment state |> List.filter (fun (_, e, _) -> e = event)
    let allEntries = yourEntries @ (opponentEntries |> List.filter (fun (_, e, _) -> e = event))

    let sorted = allEntries |> List.sortBy (fun (_, _, t) -> scoreTime t)

    let rows =
        sorted
        |> List.mapi (fun i (athlete, _, time) ->
            let place = $"{i + 1}{suffix (i + 1)}"
            let name =
                if Set.contains athlete yourRoster then $"\\textbf{{{athlete}}}" else athlete
            let team =
                if Set.contains athlete yourRoster then opt.team
                else
                    state.Rosters
                    |> Map.toSeq
                    |> Seq.tryFind (fun (teamName, members) -> members.Contains athlete)
                    |> Option.map fst
                    |> Option.defaultValue "Unknown"
            $"{place} & {name} ({team}) & {formatTime time} \\\\"
        )

    String.concat "\n" (
        [ $"\\subsection*{{Event: {event}}}"
          "\\begin{tabularx}{\\linewidth}{lXr}"
          "\\toprule"
          "Place & Athlete & Time \\\\"
          "\\midrule" ]
        @ rows @
        [ "\\bottomrule"
          "\\end{tabularx}" ]
    )

/// Constructs the full LaTeX document string for a meet
let buildOptimizationLatexDocument (state: EvalState) (optimization: Optimization) : string =
    let header = latexHeaderMeet
    let docHeader = optimizationDocHeader optimization.meet
    let eventTables = optimization.meet.Events |> List.map (optimizedEventTable state optimization)
    
    String.concat "\n\n" (
        [header]
        @ docHeader optimization
        @ eventTables
        @ ["\\end{document}"]
    )

/// Generates the meet LaTeX file using the current state and a meet ID
let generateLatexOptimization (state: EvalState) (optimization: Optimization) : string option =
    let tex = buildOptimizationLatexDocument state optimization
    Some (runPdfLatex tex "." (optimization.meet.Name + "_meet_optimization"))
        
// --------------------  Evaluation Helpers ----------------------

/// Main function for the roster show call
let rosterShow state rs =
    match generateLatexRosterShow state rs.RosterToShowName rs.Path with
    | Some path -> 
        printfn "Successfully output roster %s to: %s" rs.RosterToShowName path
        state, Some path
    | None -> RNF rs.RosterToShowName

let meetShow state ms =
    match generateLatexMeetShow state ms.MeetToShowName ms.Path with
    | Some path -> 
        printfn "Successfully output meet %s to: %s" ms.MeetToShowName path 
        state, Some path
    | None -> MNF ms.MeetToShowName

// Runs the optimization statment
let optimize (state: EvalState) (o: Optimize) =
    match Map.tryFind o.Meet state.Meets, Map.tryFind o.Team state.Rosters with
    | Some meet, Some roster -> 
            let state, optimization = runOptimization state meet roster o.Team
            match generateLatexOptimization state optimization with 
            | Some path -> 
                printfn "Successfully output the optimization for roster %s at meet %s to: %s" o.Team o.Meet path 
                state, Some path
            | None -> failwith "Error: Optimization failed."
    | Some _, None ->   RNF o.Team
    | None, Some _ ->   MNF o.Meet
    | None, None ->     failwith $"Error: Meet {o.Meet} and roster {o.Team} not found."

// ----------------------  Evaluation ----------------------

let eval (prog: Program) =
    try 
        List.fold (fun state stmt ->
            match stmt with
            | Athlete a -> declareAthlete state a
            | AthleteUpdate a -> updateAthlete state a
            | PRChange pc -> changePR state pc
            | Roster r -> declareRoster state r
            | RosterAdd ra -> addToRoster state ra
            | RosterRemoval rm -> removeFromRoster state rm
            | Meet m -> declareMeet state m
            | MeetAdd ma -> addToMeet state ma
            | MeetRemoval mr -> removeFromMeet state mr
            | Duplicate d -> duplicate state d
            | RosterShow rs -> fst (rosterShow state rs)
            | MeetShow ms -> fst (meetShow state ms)
            | Optimize o -> fst (optimize state o)
            | SetOptimizationType so -> optimizationType state so
        ) emptyState prog |> ignore
        0
    with
    | ex -> 
        printfn "%s" ex.Message
        1
