module Parser

open Combinator
open AST


/// Identifer Parser
let reserved = Set.ofList [ "prs"; "events"; "athlete"; "let"; "roster" ]

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
let rosterDecl = pseq (pright (pseq (pstr "let") (pright pws0 (pstr "roster")) (fun (_, r) -> r)) pws0) pidentifier (fun (_, name)-> {Name = name}) <!> "rosterDecl"

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

/// Statements
let athleteDeclStmt =
    athleteDecl |>> (fun a -> Athlete a) <!> "athleteDeclStmt"

let rosterDeclStmt =
    rosterDecl  |>> (fun r -> Roster r)  <!> "rosterDeclStmt"

let rosterAddStmt =
    rosterAdd |>> (fun a -> RosterAdd a) <!> "rosterAddStmt"

let psemicolon = pright pws0 (pleft (pchar ';') pws0)

let pstatement =
    athleteDeclStmt <|> rosterDeclStmt <|> rosterAddStmt <|> rosterShow


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
