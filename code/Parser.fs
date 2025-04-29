// Parser.fs — fully corrected for HeatSheet minimal syntax
namespace Parser

open System
open AST

module Parser =

    // Token definitions for HeatSheet
    type Token =
        | LET
        | ROSTER
        | ATHLETE
        | MEET
        | EVENTS
        | PRS
        | SCORING
        | MAXEVENTSPERATHLETE
        | ADD
        | TO
        | IDENT of string
        | COLON
        | COMMA
        | INT of int
        | FLOAT of float

    // Helpers to classify numeric literals
    let private isInt (s: string) =
        match Int32.TryParse(s) with
        | true, v -> Some (INT v)
        | _ -> None

    let private isFloat (s: string) =
        match Double.TryParse(s) with
        | true, v when s.Contains(".") -> Some (FLOAT v)
        | _ -> None

    // Tokenize input into a list of Token
    let tokenize (input: string) : Token list =
        // Lines without empty entries
        let lines =
            input.Split([| '\n'; '\r' |], StringSplitOptions.RemoveEmptyEntries)

        // Split on whitespace
        let rawWords =
            lines
            |> Array.collect (fun line ->
                line.Trim().Split([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries))

        // Keywords that include their own colon
        let keywordsWithColon =
            Set.ofList ["athlete:"; "meet:"; "events:"; "prs:"; "scoring:"; "maxEventsPerAthlete:"]

        // Separate trailing commas and non-keyword colons
        let words =
            rawWords
            |> Array.toList
            |> List.collect (fun w ->
                if w.EndsWith(",") then
                    let bare = w.Substring(0, w.Length - 1)
                    [ bare; "," ]
                elif w.EndsWith(":") && not (keywordsWithColon.Contains(w)) then
                    let bare = w.Substring(0, w.Length - 1)
                    [ bare; ":" ]
                else
                    [ w ])

        // Classify string tokens into Token cases
        let rec classify = function
            | [] -> []
            | ":" :: rest -> COLON :: classify rest
            | "," :: rest -> COMMA :: classify rest
            | "let" :: rest -> LET :: classify rest
            | "roster" :: rest -> ROSTER :: classify rest
            | "athlete:" :: rest -> ATHLETE :: classify rest
            | "meet:" :: rest -> MEET :: classify rest
            | "events:" :: rest -> EVENTS :: classify rest
            | "prs:" :: rest -> PRS :: classify rest
            | "scoring:" :: rest -> SCORING :: classify rest
            | "maxEventsPerAthlete:" :: rest -> MAXEVENTSPERATHLETE :: classify rest
            | "add" :: rest -> ADD :: classify rest
            | "to" :: rest -> TO :: classify rest
            | tok :: rest ->
                match isFloat tok with
                | Some f -> f :: classify rest
                | None ->
                    match isInt tok with
                    | Some i -> i :: classify rest
                    | None -> IDENT tok :: classify rest

        classify words

    // AST for declarations
    type Declaration =
        | RosterDecl of string
        | AthleteDecl of string * string list * Map<string, float>
        | MeetDecl of string * string list * Map<string, int> * int
        | AddToRoster of string * string

    // Parse a list of tokens into Declarations
    let parseProgram tokens =
        let rec loop toks acc =
            match toks with
            | [] -> List.rev acc

            // let roster <name>
            | LET :: ROSTER :: IDENT name :: rest ->
                loop rest (RosterDecl name :: acc)

            // add <athlete> to <roster>
            | ADD :: IDENT ath :: TO :: IDENT rst :: rest ->
                loop rest (AddToRoster(ath, rst) :: acc)

            // let athlete: <name>, events: ..., prs: ...
            | LET :: ATHLETE :: IDENT name :: COMMA :: EVENTS :: etoks
            | LET :: ATHLETE :: IDENT name :: EVENTS :: etoks ->
                let events, afterEvents = parseEventList etoks
                match afterEvents with
                | PRS :: prtoks ->
                    let prs, afterPrs = parsePrList prtoks
                    loop afterPrs (AthleteDecl(name, events, prs) :: acc)
                | _ -> failwithf "Expected 'prs:' after athlete events for '%s'" name

            // let meet: <name>, events: ..., scoring: ..., maxEventsPerAthlete: ...
            | LET :: MEET :: IDENT name :: COMMA :: EVENTS :: mtoks
            | LET :: MEET :: IDENT name :: EVENTS :: mtoks ->
                let events, afterEvents = parseEventList mtoks
                match afterEvents with
                | SCORING :: stokns ->
                    let scores, afterScores = parseScoreList stokns
                    match afterScores with
                    | MAXEVENTSPERATHLETE :: INT limit :: restTail ->
                        loop restTail (MeetDecl(name, events, scores, limit) :: acc)
                    | _ -> failwith "Expected 'maxEventsPerAthlete: <int>'"
                | _ -> failwith "Expected 'scoring:' after events in meet"

            | _ -> failwith "Unexpected input"

        and parseEventList tokens =
            let rec collect toks acc =
                match toks with
                | COMMA :: rest -> collect rest acc
                | IDENT name :: COMMA :: rest -> collect rest (name :: acc)
                | IDENT name :: rest -> (List.rev (name :: acc), rest)
                | _ -> (List.rev acc, toks)
            collect tokens []

        and parsePrList tokens =
            let rec collect toks acc =
                match toks with
                | PRS :: COLON :: IDENT ev :: COLON :: FLOAT t :: COMMA :: rest ->
                    // nested 'prs:' case (not expected)
                    collect rest (Map.add ev t acc)
                | IDENT ev :: COLON :: FLOAT t :: COMMA :: rest ->
                    collect rest (Map.add ev t acc)
                | IDENT ev :: COLON :: FLOAT t :: rest ->
                    (Map.add ev t acc, rest)
                | _ -> (acc, toks)
            collect tokens Map.empty

        and parseScoreList tokens =
            let rec collect toks acc =
                match toks with
                | IDENT pl :: COLON :: INT pts :: COMMA :: rest ->
                    collect rest (Map.add pl pts acc)
                | IDENT pl :: COLON :: INT pts :: rest ->
                    (Map.add pl pts acc, rest)
                | _ -> (acc, toks)
            collect tokens Map.empty

        loop tokens []

    /// Top-level parse entry
    let parse (input: string) =
        input |> tokenize |> parseProgram
