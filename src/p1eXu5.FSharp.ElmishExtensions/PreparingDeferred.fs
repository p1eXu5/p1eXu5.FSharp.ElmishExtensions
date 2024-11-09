namespace p1eXu5.FSharp.ElmishExtensions

[<RequireQualifiedAccess>]
type PreparingDeferred<'Preparing,'Retrieved> =
    | Preparing of 'Preparing
    | InProgress of 'Preparing
    | Retrieved of 'Retrieved



