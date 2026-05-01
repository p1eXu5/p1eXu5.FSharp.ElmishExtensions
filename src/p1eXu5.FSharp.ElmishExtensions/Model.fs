namespace p1eXu5.FSharp.ElmishExtensions

[<AutoOpen>]
module Model =

    /// Maps a submodel using a getter, setter, and transformation function.
    /// This is a lens composition utility for composing model updates.
    let inline map<'model,'submodel> (get: 'model -> 'submodel) (set: 'submodel -> 'model -> 'model) f model =
        model |> get |> f |> flip set model

