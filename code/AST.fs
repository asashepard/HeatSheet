module AST

open System

type Identifier = string

type Time = 
| Float of float
| MinuteTime of float * float

type Place = { Position: int; Suffix: string}

type ScoreEntry = { Place: Place; Score: int }

type PR = { Event: Identifier; Time: Time }

type AthleteDeclaration = {
    Name: Identifier
    Events: Identifier list
    PRs: PR list
}

type RosterDeclaration = {
    Name: Identifier
}

type RosterToShow = {
    RosterToShowName: Identifier
}

type RosterAdd = {
    Name: Identifier
    Roster: Identifier
}

type MeetDeclaration = {
    Name: Identifier
    Events: Identifier list
    Scoring: Place list
}

type Optimize = {
    Teams: Identifier list
    Meet: Identifier
}

type Statement =
    | Athlete of AthleteDeclaration
    | Roster of RosterDeclaration
    | RosterAdd of RosterAdd
    | RosterShow of RosterToShow
    | Meet of MeetDeclaration
    | Optimize of Optimize

type Program = Statement list
