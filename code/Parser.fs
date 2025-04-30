module Parser

open Combinator
open AST


/// Identifer Parser
let pidentifier =
    pright pws0 (pmany1 (psat (fun c -> is_letter c || is_digit c || c = '_')) |>> stringify) <!> "identifier"

/// Commas for lists
let pcomma = pright pws0 (pleft (pchar ',') pws0)

/// List of events
let peventList =
    pseq pidentifier (pmany0 (pright pcomma pidentifier))
        (fun (h, t) -> h :: t) <!> "event-list"

/// Athlete Declaration
let athleteHeader = pright (pright (pstr "let athlete") pws0) pidentifier

let athleteBody = pright (pright (pcomma) (pstr "events:")) (pright pws0 peventList)

let athleteDecl =
    pseq athleteHeader athleteBody (fun (name, events) -> { Name = name; Events = events }) <!> "athleteDecl"


/// Roster Declaration
let rosterDecl = pseq (pright (pstr "let roster") pws0) pidentifier (fun (_, name)-> {Name = name}) <!> "rosterDecl"

/// Add to Roster

let rosterAddAthlete = pseq (pright (pstr "add") pws0) (pleft pidentifier pws0) snd
let rosterAddRoster = pseq (pright (pstr "to") pws0) pidentifier snd
let rosterAdd = pseq rosterAddAthlete rosterAddRoster (fun (name, roster)-> {Name = name; Roster = roster}) <!> "rosterAdd"

let athleteDeclStmt =
    athleteDecl |>> (fun a -> Athlete a) <!> "athleteDeclStmt"

let rosterDeclStmt =
    rosterDecl  |>> (fun r -> Roster r)  <!> "rosterDeclStmt"

let rosterAddStmt =
    rosterAdd |>> (fun a -> RosterAdd a) <!> "rosterAddStmt"

let psemicolon = pright pws0 (pleft (pchar ';') pws0)

let pstatement = athleteDeclStmt <|> rosterDeclStmt <|> rosterAddStmt

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
