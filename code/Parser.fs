module Parser

open Combinator
open AST


/// Identifer Parser
let reserved = Set.ofList [ "prs"; "events"; "athlete"; "let"; "roster"; "scoring" ]
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


/// Add to Roster

let rosterAddAthlete = pseq (pright (pstr "add") pws0) (pleft pidentifier pws0) snd
let rosterAddRoster = pseq (pright (pstr "to") pws0) pidentifier snd
let rosterAdd = pseq rosterAddAthlete rosterAddRoster (fun (name, roster)-> {Name = name; Roster = roster}) <!> "rosterAdd"


/// Output Roster
let poutput =
    pright (pstr "output") pws0

let rosterShow =
    pright poutput pidentifier
    |>> (fun name -> RosterShow { RosterToShowName = name }) <!> "rosterShowStmt"

/// Meet Decl

let meetHeader = pright (pright (pseq (pstr "let") (pright pws0 (pstr "meet")) (fun (_, r) -> r)) pws0) pidentifier

let meetEvents =
    pright (pright pcomma (pstr "events:")) (pright pws0 peventList)

let pnumber = pmany0 pdigit |>> stringify

let psuffix =
    pbind (pmany1 (psat is_letter) |>> stringify) (fun s ->
        if Set.contains s suffix then presult s else pzero
    ) <!> "suffix"

let pplace = pseq (pleft (pseq pnumber psuffix (fun(x, y) -> int x)) pcolon) (pleft pnumber pws0) (fun (x, y) -> {Position = int x; Suffix = y})

let pscoringList = 
    pseq pplace (pmany0 (pright pcomma pplace))
        (fun (h, t) -> h :: t) <!> "scoring-list" 

let meetScoring =
    pright (pright pcomma (pstr "scoring:")) (pright pws0 pscoringList)

let meetTeams = pright (pright pcomma (pstr "teams:")) (pright pws0 pidentifierlist)

let optionalTeams =
    meetTeams <|> presult []


let meetBody =
    pseq meetEvents (
        pseq meetScoring optionalTeams (fun (scoring, teams) -> scoring, teams)
    ) (fun (events, (scoring, teams)) -> events, scoring, teams)



let meetDecl =
    pseq meetHeader meetBody (fun (name, (events, scoring, teams)) ->
        { Name = name; Events = events; Scoring = scoring; Teams = teams }
    )

/// Add to Meet

let meetAddTeam = pseq (pright (pstr "add") pws0) (pleft pidentifier pws0) snd
let meetAddMeet = pseq (pright (pstr "to") pws0) pidentifier snd
let meetAdd = pseq rosterAddAthlete rosterAddRoster (fun (team, meet)-> {TeamToAdd = team; Meet = meet}) <!> "rosterAdd"


/// Optimize Parser

let optimizeTeam = 
    pright (pbetween pws0 (pstr "optimize") pws0) pidentifier

let optimizeMeet = pright (pbetween pws0 (pstr "for") pws0) pidentifier

let optimize = pseq optimizeTeam optimizeMeet (fun (team, meet) -> {Team = team; Meet = meet})


/// Statements
let athleteDeclStmt =
    athleteDecl |>> (fun a -> Athlete a) <!> "athleteDeclStmt"

let rosterDeclStmt =
    rosterDecl  |>> (fun r -> Roster r)  <!> "rosterDeclStmt"

let rosterAddStmt =
    rosterAdd |>> (fun a -> RosterAdd a) <!> "rosterAddStmt"

let meetAddStmt =
    meetAdd |>> (fun a -> MeetAdd a) <!> "meetAddStmt"

let meetDeclStmt = meetDecl |>> (fun a -> Meet a) <!> "meetDeclStmt"

let optimizeStmt = optimize |>> (fun o -> Optimize o) <!> "optimizeStmt"

let psemicolon = pright pws0 (pleft (pchar ';') pws0)

let pstatement =
    athleteDeclStmt <|> rosterDeclStmt <|> rosterAddStmt <|> rosterShow <|> meetDeclStmt <|> optimizeStmt <|> meetAddStmt <|> optimizeStmt


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
