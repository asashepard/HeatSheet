module AST

open System

type Identifier = string

type Time = 
| Float of float
| MinuteTime of float * float

type ScoreEntry = { Place: int; Score: int }

type PR = { Event: Identifier; Time: Time }

type AthleteDeclaration = {
    Name: Identifier
    Events: Identifier list
    PRs: PR list
}

type AthleteUpdate = {
    UpdateName: Identifier
    NewEvents: Identifier list
    NewPRs: PR list
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
}

type MeetToShow = {
    MeetToShowName: Identifier
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

type Optimize = {
    Team: Identifier
    Meet: Identifier
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

type Program = Statement list
