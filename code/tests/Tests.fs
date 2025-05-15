module Tests

open Xunit
open Parser
open Evaluator
open AST
open Combinator

// (a) End-to-end test: parse + eval full program
[<Fact>]
let ``End-to-end: athlete and optimize`` () =
    let input = """
let roster TestRoster;
"""
    match Parser.parse input with
    | Some program ->
        let result = Evaluator.eval program
        Assert.Equal(0, result)
    | None ->
        Assert.True(false, "Parse failed")


// (b) Parser test: malformed input
[<Fact>]
let ``Parser: fails on bad syntax`` () =
    let input = "let athlete TestAthlete"
    let result = parse input
    Assert.True(result.IsNone)

// (c) Evaluator test: PRChange
[<Fact>]
let ``Evaluator: set PR works`` () =
    let prog = [
        Athlete { Name = "A"; Events = ["100m"]; PRs = []; MaxEvents = None };
        PRChange { Name = "A"; NewPR = { Event = "100m"; Time = Float 10.95 } };
    ]
    let result = eval prog
    Assert.Equal(0, result)

// (d) removing non-existent athlete causes run-time error
[<Fact>]
let ``Removing unknown athlete causes error`` () =
    let prog = [
        Roster { Name = "R"; Athletes = [] };
        RosterRemoval { AthleteToRemove = "Ghost"; Roster = "R" };
    ]
    let result = eval prog
    Assert.Equal(1, result)

// (e) duplicate athlete
[<Fact>]
let ``Duplicate athlete works`` () =
    let prog = [
        Athlete { Name = "A"; Events = ["200m"]; PRs = [{ Event = "200m"; Time = Float 22.0 }]; MaxEvents = None };
        Duplicate (DuplicateAthlete { AthleteToDuplicate = "A"; NewIdentifier = "A2" })
    ]
    let result = eval prog
    Assert.Equal(0, result)
