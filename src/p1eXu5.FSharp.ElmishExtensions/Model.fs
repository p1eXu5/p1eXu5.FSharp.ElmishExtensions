namespace p1eXu5.FSharp.ElmishExtensions

[<AutoOpen>]
module Model =

    /// Maps a submodel using a getter, setter, and transformation function.
    /// This is a lens composition utility for composing model updates.
    let inline map<'model,'submodel> (get: 'model -> 'submodel) (set: 'submodel -> 'model -> 'model) (f: 'submodel -> 'submodel) model =
        model |> get |> f |> flip set model

    /// Maps a submodel and returns both the updated submodel and an intent.
    /// Applies a transformation function that produces both a submodel and an intent value.
    let inline mapIntent<'model,'submodel,'subintent>
        (get: 'model -> 'submodel)
        (set: 'submodel -> 'model -> 'model)
        (f: 'submodel -> 'submodel * 'subintent)
        model
        =
        let (sm, intent) = model |> get |> f
        let model' = sm |> flip set model
        model', intent

    /// Maps a submodel, extracts an intent, and handles it with the provided handler function.
    /// Combines submodel mapping with intent handling in a single operation.
    let inline mapHandleIntent<'model,'submodel,'subintent>
        (get: 'model -> 'submodel)
        (set: 'submodel -> 'model -> 'model)
        (f: 'submodel -> 'submodel * 'subintent)
        (handleIntentf: 'subintent -> 'model -> 'model)
        model
        =
        let (sm, intent) = model |> get |> f
        let model' = sm |> flip set model
        model' |> handleIntentf intent

    open Elmish

    /// Maps a submodel and returns both the updated submodel and an subcommand.
    /// Applies a transformation function that produces both a submodel and an subcommand value.
    let inline mapIdCmd<'model,'submodel,'cmd,'subMsg,'id>
        (get: 'model -> 'submodel)
        (set: 'submodel -> 'model -> 'model)
        (f: 'submodel -> 'submodel * Cmd<'subMsg>)
        (getId: 'submodel -> 'id)
        (mapSubMsg: 'id * 'subMsg -> 'cmd)
        model
        =
        let (sm, cmd) = model |> get |> f
        let model' = sm |> flip set model
        model', Cmd.map (fun smsg -> mapSubMsg (getId sm, smsg)) cmd

    /// Maps a submodel and returns both the updated submodel and an subcommand.
    /// Applies a transformation function that produces both a submodel and an subcommand value.
    let inline mapCmd<'model,'submodel,'cmd,'subMsg>
        (get: 'model -> 'submodel)
        (set: 'submodel -> 'model -> 'model)
        (f: 'submodel -> 'submodel * Cmd<'subMsg>)
        (mapSubMsg: 'subMsg -> 'cmd)
        model
        =
        let (sm, cmd) = model |> get |> f
        let model' = sm |> flip set model
        model', Cmd.map mapSubMsg cmd

