module PRPredictor =

    /// Generic time-scaling helper
    let scaleTime (fromDist: float) (toDist: float) (time: float) (adjustment: float) : float =
        let speed = fromDist / time
        toDist / (speed / adjustment)

    // Specific converters
    let predict200mFrom100m  t100  = scaleTime 100.0 200.0 t100 1.03
    let predict100mFrom200m  t200  = scaleTime 200.0 100.0 t200 1.03
    let predict400mFrom200m  t200  = scaleTime 200.0 400.0 t200 1.08
    let predict200mFrom400m  t400  = scaleTime 400.0 200.0 t400 1.08

    /// Core lookup: best guess time (actual PR or estimated)
    let estimatePR (athlete: AthleteDeclaration) (event: string) : float option =
        let lookup e =
            athlete.PRs
            |> List.tryFind (fun pr -> pr.Event = e)
            |> Option.map (fun pr ->
                match pr.Time with
                | Float f -> f
                | MinuteTime (m, s) -> 60.0 * float m + s)

        match event with
        | "100m" -> lookup "100m" <|> (lookup "200m" |> Option.map predict100mFrom200m)
        | "200m" -> lookup "200m"
                    <|> (lookup "100m" |> Option.map predict200mFrom100m)
                    <|> (lookup "400m" |> Option.map predict200mFrom400m)
        | "400m" -> lookup "400m" <|> (lookup "200m" |> Option.map predict400mFrom200m)
        | _      -> lookup event