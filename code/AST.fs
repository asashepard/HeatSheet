module AST

open System

type Identifier = string

type Time = 
| Float of float
| MinuteTime of float * float

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

type Statement =
    | Athlete of AthleteDeclaration
    | Roster of RosterDeclaration
    | RosterAdd of RosterAdd
    | RosterShow of RosterToShow

type Program = Statement list
