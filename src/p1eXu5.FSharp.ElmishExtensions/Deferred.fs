namespace p1eXu5.FSharp.ElmishExtensions

[<RequireQualifiedAccess>]
type Deferred<'Retrieved> =
    | NotRequested
    | InProgress
    | Retrieved of 'Retrieved



