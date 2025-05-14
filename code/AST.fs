module AST

open System

type Identifier = string

type Time = 
| Float of float
| MinuteTime of float * float
| HourMinuteTime of float * float * float

type OptimizationType =
| Basic
| Simulation

type ScoreEntry = { Place: int; Score: int }

type PR = { Event: Identifier; Time: Time }

type AthleteDeclaration = {
    Name: Identifier
    Events: Identifier list
    PRs: PR list
    MaxEvents: int option
}



type AthleteUpdate = {
    UpdateName: Identifier
    NewEvents: Identifier list
    NewPRs: PR list
    NewMaxEvents: int option
}


type SetPR = {
    Name: Identifier
    NewPR: PR
}

type RosterDeclaration = {
    Name: Identifier
    Athletes: Identifier list
}


type RosterToShow = {
    RosterToShowName: Identifier
    Path: string option
}

type MeetToShow = {
    MeetToShowName: Identifier
    Path: string option
}

type RosterAdd = {
    AthleteToAdd: Identifier
    Roster: Identifier
}

type RosterRemoval = {
    AthleteToRemove: Identifier
    Roster: Identifier
}

type MeetAdd = {
    TeamToAdd: Identifier
    Meet: Identifier
}

type MeetRemoval = {
    TeamToRemove: Identifier
    Meet: Identifier
}


type MeetDeclaration = {
    Name: Identifier
    Events: Identifier list
    Scoring: ScoreEntry list
    Teams: Identifier list
    MaxAthletesPerEvent: int option
}

type DuplicateAthlete = {
    AthleteToDuplicate: Identifier
    NewIdentifier: Identifier
}

type DuplicateMeet = {
    MeetToDuplicate: Identifier
    NewIdentifier: Identifier
}


type DuplicateRoster = {
    RosterToDuplicate: Identifier
    NewIdentifier: Identifier
}

type Duplicate = 
| DuplicateAthlete of DuplicateAthlete
| DuplicateRoster of DuplicateRoster
| DuplicateMeet of DuplicateMeet


type Optimize = {
    Team: Identifier
    Meet: Identifier
}

type ForceAthlete = {
    AthleteName: Identifier
    EventToForce: Identifier
}

type FreeAthlete = {
    AthleteToFree: Identifier
    EventToFree: Identifier
}

type Statement =
    | Athlete of AthleteDeclaration
    | AthleteUpdate of AthleteUpdate
    | PRChange of SetPR
    | RosterRemoval of RosterRemoval
    | Roster of RosterDeclaration
    | RosterAdd of RosterAdd
    | RosterShow of RosterToShow
    | MeetShow of MeetToShow
    | Meet of MeetDeclaration
    | Optimize of Optimize
    | MeetAdd of MeetAdd
    | MeetRemoval of MeetRemoval
    | Duplicate of Duplicate
    | SetOptimizationType of OptimizationType
    | ForceAthleteToEvent of ForceAthlete
    | FreeAthleteFromEvent of FreeAthlete

type Program = Statement list
