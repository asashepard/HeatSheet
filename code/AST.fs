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

type RosterDeclaration = {
    Name: Identifier
    Athletes: Identifier list
}


type RosterToShow = {
    RosterToShowName: Identifier
}

type RosterAdd = {
    Name: Identifier
    Roster: Identifier
}

type MeetAdd = {
    TeamToAdd: Identifier
    Meet: Identifier
}

type MeetDeclaration = {
    Name: Identifier
    Events: Identifier list
    Scoring: ScoreEntry list
    Teams: Identifier list
}

type Optimize = {
    Team: Identifier
    Meet: Identifier
}

type Statement =
    | Athlete of AthleteDeclaration
    | Roster of RosterDeclaration
    | RosterAdd of RosterAdd
    | RosterShow of RosterToShow
    | Meet of MeetDeclaration
    | Optimize of Optimize
    | MeetAdd of MeetAdd

type Program = Statement list
