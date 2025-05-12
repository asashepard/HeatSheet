module Parser

open Combinator
open AST


/// Identifer Parser
let reserved = Set.ofList [ "prs"; "events"; "athlete"; "let"; "roster"; "scoring"; "include"; "in"; "to"; "remove"; "from"; "maxEventsPerAthlete"]
let suffix = Set.ofList [ "st"; "nd"; "rd"; "th" ]

let pidentifier =
    let baseId =
        pmany1 (psat (fun c -> is_letter c || is_digit c || c = '_')) 
        |>> stringify
    
    pright pws0 (
        pbind baseId (fun id ->
            if Set.contains id reserved then pzero else presult id
        )
    ) <!> "identifier"

/// Commas for lists
let pcomma = pbetween pws0 (pchar ',') pws0

let pidentifierlist = 
    pseq pidentifier (pmany0 (pright pcomma pidentifier)) (fun (first, rest) -> first :: rest)


/// Padder
let pad p = pbetween pws0 p pws0

/// List of events
let peventList =
    pseq pidentifier (pmany0 (pright pcomma pidentifier))
        (fun (h, t) -> h :: t) <!> "event-list"

let pcolon = pbetween pws0 (pchar ':') pws0

let pint: Parser<float> =
    pmany1 (psat is_digit) |>> (stringify >> float)

let pfloat: Parser<float> =
    pseq
        (pmany1 (psat is_digit) |>> stringify)
        (pright (pchar '.') (pmany1 (psat is_digit) |>> stringify))
        (fun (i, d) -> float (i + "." + d))

let pseconds: Parser<Time> =
    (pfloat |>> Float) <|> (pint |>> Float)

let pminutetime: Parser<Time> =
    pseq pint (pright (pchar ':') (pfloat <|> pint))
        (fun (min, sec) -> MinuteTime(min, sec))

let ptime: Parser<Time> =
    pminutetime <|> pseconds <!> "ptime"

let prentry: Parser<PR> =
    pseq pidentifier (pright pcolon ptime)
        (fun (event, time) -> { Event = event; Time = time }) <!> "prentry"

let prList: Parser<PR list> =
    pseq prentry (pmany0 (pright pcomma prentry))
        (fun (h, t) -> h :: t) <!> "prList"


/// Athlete Declaration
let athleteHeader = pright (pright (pseq (pstr "let") (pright pws0 (pstr "athlete")) (fun (_, r) -> r)) pws0) pidentifier

let athleteEvents =
    pright (pright pcomma (pstr "events:")) (pright pws0 peventList)

let athletePRs =
    pright (pright pcomma (pstr "prs:")) (pright pws0 prList)


let athleteBody =
    pseq athleteEvents athletePRs id

let athleteDecl =
    pseq athleteHeader athleteBody
         (fun (name, (events, prs)) -> { Name = name; Events = events; PRs = prs }) <!> "athleteDecl"


/// Athlete Update
let athleteUpdateHeader = pright (pright (pseq (pstr "update") (pright pws0 (pstr "athlete")) (fun (_, r) -> r)) pws0) pidentifier

let athleteUpdate =
    pseq athleteUpdateHeader athleteBody
         (fun (name, (events, prs)) -> { UpdateName = name; NewEvents = events; NewPRs = prs }) <!> "athleteUpdate"


/// Change a PR

let athleteToChangePR = pright (pad (pstr "set")) pidentifier

let timeToChangePR = pright (pad (pstr "to")) ptime

let eventToChangePR = pright (pad (pstr "in")) pidentifier

let changePR = pseq athleteToChangePR (pseq timeToChangePR eventToChangePR id) (fun(athlete, eventTime) -> {Name = athlete; NewPR = {Event = snd eventTime; Time = fst eventTime}}) 

/// Roster Declaration

let rosterAthletes = pright (pright pcomma (pstr "athletes:")) (pright pws0 pidentifierlist)

let optionalAthletes = rosterAthletes <|> presult []

let rosterHeader =
    pright
        (pseq (pstr "let") (pright pws0 (pstr "roster")) (fun (_, r) -> r))
        pws0

let rosterDecl =
    pseq (pseq rosterHeader pidentifier (fun (_, name) -> name)) optionalAthletes
        (fun (name, athletes) -> { Name = name; Athletes = athletes }) <!> "rosterDecl"


/// Add to and remove from roster

let rosterAddAthlete = pseq (pright (pstr "add") pws0) (pleft pidentifier pws0) snd
let rosterToAddTo = pseq (pright (pstr "to") pws0) pidentifier snd
let rosterAdd = pseq rosterAddAthlete rosterToAddTo (fun (name, roster)-> RosterAdd{AthleteToAdd = name; Roster = roster}) <!> "rosterAdd"

let rosterRemoveAthlete = pseq (pad (pstr "remove")) (pad pidentifier) snd
let rosterToRemoveFrom = pseq (pad (pstr "from") ) (pad pidentifier) snd
let rosterRemoval = pseq rosterRemoveAthlete rosterToRemoveFrom (fun (name, roster)-> RosterRemoval{AthleteToRemove = name; Roster = roster}) <!> "rosterRemoval"

/// Output Roster
let poutput =
    pright (pad (pstr "output")) pws0

let rosterShow =
    pright (pright poutput (pad (pstr "roster"))) pidentifier
    |>> (fun name -> RosterShow { RosterToShowName = name }) <!> "rosterShowStmt"

/// Output Meet
let meetShow =
    pright (pright poutput (pad (pstr "meet"))) pidentifier
    |>> (fun name -> MeetShow { MeetToShowName = name }) <!> "meetShowStmt"

/// Meet Decl

let meetHeader = pright (pright (pseq (pstr "let") (pright pws0 (pstr "meet")) (fun (_, r) -> r)) pws0) pidentifier

let meetEvents =
    pright (pright pcomma (pstr "events:")) (pright pws0 peventList)

let pnumber = pmany0 pdigit |>> stringify

let psuffix =
    pbind (pmany1 (psat is_letter) |>> stringify) (fun s ->
        if Set.contains s suffix then presult s else pzero
    ) <!> "suffix"

let pplace = pseq (pleft (pseq pnumber psuffix (fun(x, y) -> int x)) pcolon) (pleft pnumber pws0) (fun (x, y) -> {Place = int x; Score = int y})

let pscoringList = 
    pseq pplace (pmany0 (pright pcomma pplace))
        (fun (h, t) -> h :: t) <!> "scoring-list" 

let meetScoring =
    pright (pright pcomma (pstr "scoring:")) (pright pws0 pscoringList)

let meetTeams = pright (pright pcomma (pstr "teams:")) (pright pws0 pidentifierlist)

let optionalTeams =
    meetTeams <|> presult []

let optionalMaxAthletes = 
    pright (pright pcomma (pstr "maxEventsPerAthlete:")) (pright pws0 pnumber)
    |>> (fun n -> Some(int n))
    <|> presult None


let meetDeclarationOptionals = pseq optionalTeams optionalMaxAthletes id

let meetBody =
    pseq meetEvents (
        pseq meetScoring meetDeclarationOptionals (fun (scoring, optionals) -> scoring, optionals)
    ) (fun (events: string list, (scoring, optionals)) -> events, scoring, fst optionals, snd optionals)


let meetDecl =
    pseq meetHeader meetBody (fun (name, (events, scoring, teams, maxEventsPerAthlete)) ->
        { Name = name; Events = events; Scoring = scoring; Teams = teams; MaxAthletesPerEvent = maxEventsPerAthlete}
    )

/// Add to Meet

let meetAddTeam = pseq (pad (pstr "include")) (pleft pidentifier pws0) snd
let meetToAddTo = pseq (pright (pstr "in") pws0) pidentifier snd
let meetAdd = pseq meetAddTeam meetToAddTo (fun (team, meet)-> {TeamToAdd = team; Meet = meet}) <!> "meetAdd"

/// Remove from Meet

let meetRemoveTeam = pseq (pad (pstr "exclude")) (pleft pidentifier pws0) snd


/// Optimizer

let optimizeTeam = 
    pright (pbetween pws0 (pstr "optimize") pws0) pidentifier

let optimizeMeet = pright (pbetween pws0 (pstr "for") pws0) pidentifier

let optimize = pseq optimizeTeam optimizeMeet (fun (team, meet) -> {Team = team; Meet = meet})


/// Statements
let athleteDeclStmt = athleteDecl |>> Athlete <!> "athleteDeclStmt"
let rosterDeclStmt = rosterDecl  |>> Roster  <!> "rosterDeclStmt"
let rosterAddStmt = rosterAdd <!> "rosterAddStmt"

let rosterRemoveStmt = rosterRemoval <!> "rosterRemoveStmt"

let meetAddStmt = meetAdd |>> MeetAdd <!> "meetAddStmt"
let athleteUpdateStmt = athleteUpdate |>> AthleteUpdate <!> "athleteUpdateStmt"
let PRChangeStmt = changePR |>> PRChange <!> "prChangeStmt"
let meetDeclStmt = meetDecl |>> Meet <!> "meetDeclStmt"
let optimizeStmt = optimize |>> Optimize <!> "optimizeStmt"

let psemicolon = pright pws0 (pleft (pchar ';') pws0)

let pstatement =
    athleteDeclStmt <|> rosterDeclStmt <|> rosterAddStmt <|> rosterShow <|> 
    meetDeclStmt <|> optimizeStmt <|> meetAddStmt <|> optimizeStmt <|> 
    athleteUpdateStmt <|> PRChangeStmt <|> rosterRemoveStmt <|> meetShow



let programParser =
    pleft
        (pseq pstatement (pmany0 (pright psemicolon pstatement)) (fun (s, rest) -> s :: rest)) psemicolon


let parse s =
    let prog = prepare s
    let parser = pleft programParser (pright pws0 peof)
    match parser prog with
    | Success(p, _) -> Some p
    | Failure(pos, rule) ->
        let msg = diagnosticMessage 20 pos (input prog) rule
        printfn "Parse error:\n%s" msg
        None
