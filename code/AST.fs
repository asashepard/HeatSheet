module AST

open System

type Identifier = string

type AthleteDeclaration = {
    Name: Identifier
    Events: Identifier list
}

type RosterDeclaration = {
    Name: Identifier
}

type RosterAdd = {
    Name: Identifier
    Roster: Identifier
}

type Statement =
    | Athlete of AthleteDeclaration
    | Roster of RosterDeclaration
    | RosterAdd of RosterAdd

type Program = Statement list
