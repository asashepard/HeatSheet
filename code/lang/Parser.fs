module Parser

open Combinator
open AST


/// Identifer Parser
let reserved = Set.ofList [
  "let"; "roster"; "athlete"; "update"; "events"; "prs"; "scoring"; "teams"; 
  "maxEntries"; "include"; "exclude"; "add"; "remove"; "free";
  "from"; "to"; "in"; "output"; "optimize"; "for"; "set"; "maxEvents"; "force"
]
/// List of valid suffixes for score entries
let suffix = Set.ofList [ "st"; "nd"; "rd"; "th" ]

/// Parses all whitespaces surrouding p
let pad p = pbetween pws0 p pws0

/// Commas for lists
let pcomma = pad (pchar ',')

/// Parses a colon and pads for whitespace around it
let pcolon = pad (pchar ':')

/// Parses a semicolon and pads for whitespace around it
let psemicolon = pad (pchar ';')

/// Parses a list from a single item parser
let plist pelement = pseq pelement (pmany0 (pright pcomma pelement)) (fun (a, b) -> a :: b)

/// Parses language identifiers
let pidentifier =
    let baseId =
        pmany1 (psat (fun c -> is_letter c || is_digit c || c = '_')) 
        |>> stringify
    
    pright pws0 (
        pbind baseId (fun id ->
            if Set.contains id reserved then pzero else presult id
        )
    ) <!> "identifier"

/// Parses a list of identifiers
let identifierList = plist pidentifier <!> "identifierList"

/// Parses a number
let pnumber = pmany1 pdigit |>> stringify

/// Pareses an integer into a float value for timing
let pint: Parser<float> = pnumber |>> float

/// Parses a float into a float value for timing
let pfloat: Parser<float> =
    pseq
        pnumber
        (pright (pchar '.') pnumber)
        (fun (i, d) -> float (i + "." + d))

/// Parses seconds
let pseconds = pfloat |>> Float <|> (pint |>> Float)

/// Parses a complete minute time e.g. 1:54.00
let minutetime = pseq pint (pright pcolon (pfloat <|> pint)) MinuteTime

/// Parses a complete hour time e.g. 1:53:24.00
let hourMinuteTime =
    pseq pint (pright pcolon minutetime) (fun (h, t) ->
        match t with
        | MinuteTime (m, s) -> HourMinuteTime(h, int m, s)
        | _ -> failwith "Unreachable: minutetime only returns MinuteTime"
    )

/// Complete parser for all valid time inputs
let time = hourMinuteTime <|> minutetime <|> pseconds <!> "ptime"

/// Parses a PR entry in the form of "100m : 11.01"
let prentry: Parser<PR> =
    pseq pidentifier (pright pcolon time) 
        (fun (event, time) -> { Event = event; Time = time }) <!> "prentry"

/// Parses a full list of PR entries
let prList = plist prentry

/// Athlete Declaration
let athleteHeader = pright (pright (pseq (pstr "let") (pright pws0 (pstr "athlete")) (fun (_, r) -> r)) pws0) pidentifier

/// Parses a list of identifers, which are the athlete's events
let eventList = pright (pright pcomma (pright (pstr "events") pcolon)) (pad identifierList)

/// Parses the list of athlete's PRs
let athletePRs = pright (pright pcomma (pright (pstr "prs") pcolon)) (pad prList)

/// Makes prList optional for athletes
let athletePRsOptional = athletePRs <|> presult []

/// Parses the max events for the athlete
let athleteMaxEvents = pright (pright pcomma (pright (pad (pstr "maxEvents")) pcolon)) pnumber

/// Makes max events optional 
let athleteMaxEventsOptional = athleteMaxEvents |>> int |>> Some <|> presult None

/// Parses the athletes body, including events and PRs
let athleteBody = pseq eventList (pseq athletePRsOptional athleteMaxEventsOptional id) id

/// Parses a full athlete declaration
let athleteDecl =
    pseq athleteHeader athleteBody
         (fun (name, (events, (prs, maxEvents))) -> { Name = name; Events = events; PRs = prs; MaxEvents = maxEvents }) <!> "athleteDecl"

/// parser for athlete update header
let athleteUpdateHeader = pright (pad (pseq (pstr "update") (pad (pstr "athlete")) snd)) pidentifier

/// Parsees the entire athlete update
let athleteUpdate =
    pseq athleteUpdateHeader athleteBody
         (fun (name, (events, (prs, maxEvents))) -> { UpdateName = name; NewEvents = events; NewPRs = prs; NewMaxEvents = maxEvents }) <!> "athleteUpdate"


/// parses an athletes name whose PR we are changing, based on keyword "set"
let athleteToChangePR = pright (pad (pstr "set")) pidentifier

/// parses the time we are changing the PR to, based on keyword "to"
let timeToChangePR = pright (pad (pstr "to")) time

/// Parses the event that we are changing the PR in, based on keyword "in"
let eventToChangePR = pright (pad (pstr "in")) pidentifier

/// Parses the complete PR change
let changePR = pseq athleteToChangePR (pseq timeToChangePR eventToChangePR id) (
    fun(athlete, eventTime) -> 
        {Name = athlete; NewPR = {Event = snd eventTime; Time = fst eventTime}}
    )

/// Parses the athlete declaration for a roster
let rosterAthletes = pright (pright pcomma (pright  (pstr "athletes") pcolon)) (pad identifierList)

/// Makes it optional to have to put in the roster's athletes
let optionalAthletes = rosterAthletes <|> presult []

/// Parses the main keywords of the roster
let rosterHeader = pad (pseq (pad (pstr "let")) (pad (pstr "roster")) snd)

/// Parses the full roster declaration
let rosterDecl =
    pseq (pseq rosterHeader pidentifier snd) optionalAthletes
        (fun (name, athletes) -> { Name = name; Athletes = athletes }) <!> "rosterDecl"

/// Parses the athlete to add to the roster
let rosterAddAthlete = pseq (pad (pstr "add")) (pad pidentifier) snd

/// Parses the roster that we are adding to
let rosterToAddTo = pseq (pad (pstr "to")) (pad pidentifier) snd

/// Parses a complete roster add
let rosterAdd = pseq rosterAddAthlete rosterToAddTo (fun (name, roster)-> RosterAdd{AthleteToAdd = name; Roster = roster}) <!> "rosterAdd"

/// Parses the athlete to remove from the roster
let rosterRemoveAthlete = pseq (pad (pstr "remove")) (pad pidentifier) snd

/// Parses the roster that we are removing from
let rosterToRemoveFrom = pseq (pad (pstr "from") ) (pad pidentifier) snd

/// Parses the complete roster removal
let rosterRemoval = pseq rosterRemoveAthlete rosterToRemoveFrom (fun (name, roster)-> RosterRemoval{AthleteToRemove = name; Roster = roster}) <!> "rosterRemoval"

/// Parses output keyword
let poutput =pad (pstr "output")

/// Parses characters that can be used in a file path
let is_path_char c =
    is_letter c || is_digit c || c = '/' || c = '.' || c = '_' || c = '-'

/// Parses a file path
let ppath = pbetween (pright pws0 (pchar '"')) (pmany1 (psat is_path_char)) (pright pws0 (pchar '"')) |>> stringify

/// Output to this path
let outputPath = pright (pad (pstr "to")) ppath

/// Makes the file path optional
let pathOptional = outputPath |>> Some <|> presult None

/// Output roster
let rosterShow =
    pseq (pright (pright poutput (pad (pstr "roster"))) pidentifier) pathOptional
        (fun (name, path) -> RosterShow { RosterToShowName = name; Path = path }) <!> "rosterShowStmt"

/// Output meet
let meetShow =
    pseq (pright (pright poutput (pad (pstr "meet"))) pidentifier) pathOptional
        (fun (name, path) -> MeetShow { MeetToShowName = name; Path = path }) <!> "meetShowStmt"


/// Parses meet declaration keywords
let meetHeader = pright (pseq (pad (pstr "let")) (pad (pstr "meet")) snd) (pad pidentifier)

/// Parses the suffix for a place e.g. "3rd" -> this parses the "rd"
let psuffix =
    pbind (pmany1 (psat is_letter) |>> stringify) (fun s ->
        if Set.contains s suffix then presult s else pzero
    ) <!> "suffix"

/// Parses the place : score combo
let pplace = pseq (pleft (pseq (pad pnumber) psuffix (fun(x, y) -> int x)) pcolon) (pad pnumber) (fun (x, y) -> {Place = int x; Score = int y})

/// Parses the complete scoring list
let pscoringList = plist pplace <!> "scoring-list" 

/// Parses meet scoring
let meetScoring = pright (pright pcomma (pright(pad (pstr "scoring")) pcolon)) (pad pscoringList)

/// Parses meet teams
let meetTeams = pright (pright pcomma (pright(pad (pstr "teams")) pcolon)) (pad identifierList)

/// Makes including meet teams optional
let optionalTeams = meetTeams <|> presult []

/// Parses maxEventsPerAthlete
let maxAthletes = pright (pright pcomma (pright (pad(pstr "maxEntries")) pcolon)) (pad pnumber) |>> int |>> Some

/// Makes it optional to include max athletes
let optionalMaxAthletes = maxAthletes <|> presult None

/// All the optional fields for a meet
let meetDeclarationOptionals = pseq optionalTeams optionalMaxAthletes id

/// Parses the complete body of a meet declaration
let meetBody =
    pseq eventList (pseq meetScoring meetDeclarationOptionals id) 
        ( fun (events: string list, (scoring, optionals)) -> events, scoring, fst optionals, snd optionals)

/// Parses a complete meet declaration
let meetDecl =
    pseq meetHeader meetBody (fun (name, (events, scoring, teams, maxEventsPerAthlete)) ->
        { Name = name; Events = events; Scoring = scoring; Teams = teams; MaxAthletesPerEvent = maxEventsPerAthlete}
    )

/// Parses team to add to meet
let meetAddTeam = pseq (pad (pstr "include")) (pad pidentifier) snd

/// Parses the meet to add the team to
let meetToAddTo = pseq (pad (pstr "in")) (pad pidentifier) snd

/// Parses a complete add to meet statement
let meetAdd = pseq meetAddTeam meetToAddTo (fun (team, meet)-> {TeamToAdd = team; Meet = meet}) <!> "meetAdd"

/// Parses team to remove from meet
let meetRemoveTeam = pseq (pad (pstr "exclude")) (pleft pidentifier pws0) snd

/// Parses the meet to remove the team from
let meetToRemoveFrom = pseq (pad (pstr "from")) (pad pidentifier) snd

/// Parses a complete remove from meet statement
let meetRemoval = pseq meetRemoveTeam meetToRemoveFrom (fun (team, meet)-> {TeamToRemove = team; Meet = meet}) <!> "meetAdd"

/// Parses the word duplicate
let duplicate = pad (pstr "duplicate")

/// Parses the identifer that we duplicate to
let thingToDuplicate = pright(pad(pstr "to")) (pad pidentifier)

/// Parses an athlete duplication
let duplicateAthlete = pseq (pright duplicate (pad(pstr "athlete"))) pidentifier id

/// Parses a roster duplication
let duplicateRoster = pseq (pright duplicate (pad(pstr "roster"))) pidentifier id

/// Parses a meet duplication
let duplicateMeet = pseq (pright duplicate (pad(pstr "meet"))) pidentifier id

/// Parses the type of duplication
let duplicationType = duplicateAthlete <|> duplicateRoster <|> duplicateMeet

/// Parses the actual duplication statement
let duplication = pseq duplicationType thingToDuplicate id

/// Parses the team to optimize
let optimizeTeam = pright (pad (pstr "optimize")) pidentifier

/// Parses the meet to optimize for
let optimizeMeet = pright (pad (pstr "for")) pidentifier

/// Parses complete optimize statement
let optimize = pseq optimizeTeam optimizeMeet (fun (team, meet) -> {Team = team; Meet = meet})

/// Parses set optimization type
let optimizationType = pad(pstr "basic") <|> pad(pstr "simulation")

/// Parses the header for an optimization type
let optimizationTypeHeader = pright (pright (pad(pstr "set")) (pad(pstr "optimization"))) (pright (pad(pstr "type")) (pad(pstr "to"))) 

/// Parses a complete set optimization type
let setOptimizationType = pseq optimizationTypeHeader optimizationType (
        fun (_, t) ->
            match t with 
            | "basic" -> Basic
            | "simulation" -> Simulation
            | _ -> failwith "Unreachable: optimizationType only returns basic or simulation. "
    )

/// Parses athlete to force
let forceAthlete = pright (pad (pstr "force") ) (pad pidentifier)

/// Parses event to force athlete to
let forceEvent = pright (pad (pstr "to") ) (pad pidentifier)

/// Parses the complete force statement
let forceAthleteToEvent = pseq forceAthlete forceEvent (fun(a, e) -> {AthleteName = a; EventToForce = e})

/// Parses athlete to free
let freeAthlete = pright (pad (pstr "free") ) (pad pidentifier)

/// Parses event to free athlete to
let freeEvent = pright (pad (pstr "from") ) (pad pidentifier)

/// Parses the complete free statement
let freeAthleteFromEvent = pseq freeAthlete freeEvent (fun(a, e) -> {AthleteToFree = a; EventToFree = e})

/// All of the possible statements in the language
let athleteDeclStmt = athleteDecl |>> Athlete <!> "athleteDeclStmt"
let rosterDeclStmt = rosterDecl  |>> Roster  <!> "rosterDeclStmt"
let rosterAddStmt = rosterAdd <!> "rosterAddStmt"
let rosterRemoveStmt = rosterRemoval <!> "rosterRemoveStmt"
let meetAddStmt = meetAdd |>> MeetAdd <!> "meetAddStmt"
let athleteUpdateStmt = athleteUpdate |>> AthleteUpdate <!> "athleteUpdateStmt"
let PRChangeStmt = changePR |>> PRChange <!> "prChangeStmt"
let meetDeclStmt = meetDecl |>> Meet <!> "meetDeclStmt"
let meetRemoveStmt = meetRemoval |>> MeetRemoval <!> "meetRemoveStmt"
let optimizeStmt = optimize |>> Optimize <!> "optimizeStmt"
let setOptimizationTypeStmt = setOptimizationType |>> SetOptimizationType <!> "optimizationTypeStmt"
let forceAthleteToEventStmt = forceAthleteToEvent |>> ForceAthleteToEvent <!> "forceAthleteToEventStmt"
let freeAthleteFromEventStmt = freeAthleteFromEvent |>> FreeAthleteFromEvent <!> "freeAthleteFromEventStmt"
let duplicationStmt =
    duplication |>> (fun ((typ, orig), newName) ->
        match typ with
        | "athlete" -> Duplicate(DuplicateAthlete {AthleteToDuplicate = orig; NewIdentifier = newName})
        | "roster" -> Duplicate(DuplicateRoster {RosterToDuplicate = orig; NewIdentifier = newName})
        | "meet" -> Duplicate(DuplicateMeet {MeetToDuplicate = orig; NewIdentifier = newName})
        | _ -> failwith "unreachable"  // typ only comes from duplicationType, so this is safe
    ) <!> "duplicationStmt"

let pstatement =
    athleteDeclStmt <|> rosterDeclStmt <|> rosterAddStmt <|> rosterShow <|> forceAthleteToEventStmt <|>
    meetDeclStmt <|> optimizeStmt <|> meetAddStmt <|> optimizeStmt <|> setOptimizationTypeStmt <|>
    athleteUpdateStmt <|> PRChangeStmt <|> rosterRemoveStmt <|> meetShow <|> duplicationStmt <|> freeAthleteFromEventStmt

/// Parses a list of statements, requiring semicolons after each
let programParser =
    pseq (pleft pstatement psemicolon) (pmany0 (pleft pstatement psemicolon)) (fun (x, xs) -> x :: xs)

/// Final grammar
let grammar = pleft programParser peof

let parse s =
    let prog = prepare s
    match grammar prog with
    | Success(p, _) -> Some p
    | Failure(pos, rule) ->
        let msg = diagnosticMessage 20 pos (input prog) rule
        printfn "Parse error:\n%s" msg
        None
