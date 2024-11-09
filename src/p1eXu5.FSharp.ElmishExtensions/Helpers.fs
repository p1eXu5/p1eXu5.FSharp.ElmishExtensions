namespace p1eXu5.FSharp.ElmishExtensions

open Elmish

[<AutoOpen>]
module Helpers =

    let withCmdNone = fun m -> m, Cmd.none
    
    let flip f b a = f a b


[<AutoOpen>]
module Model =

    let map get set f model =
        model |> get |> f |> flip set model


[<AutoOpen>]
module List =

    let mapFirst predicate updatef modelList =
        let rec mapFirstRec reverseFront back =
            match back with
            | [] ->
                (*
                    * Conceptually, the correct value to return is
                    * reverseFront |> List.rev
                    * but this is the same as
                    * input
                    * so returning that instead.
                    *)
                modelList
            | a :: ma ->
                if predicate a then
                    (reverseFront |> List.rev) @ (updatef a :: ma)
                else
                    mapFirstRec (a :: reverseFront) ma
        mapFirstRec [] modelList

    let mapFirstCmd predicate updatef modelList =
        let rec mapFirstRec reverseFront cmd back =
            match back with
            | [] ->
                (*
                 * Conceptually, the correct value to return is
                 * reverseFront |> List.rev
                 * but this is the same as
                 * input
                 * so returning that instead.
                 *)
                modelList, cmd
            | model :: tailModels ->
                if predicate model then
                    let (model, cmd) = updatef model
                    (reverseFront |> List.rev) @ (model :: tailModels), cmd
                else
                    mapFirstRec (model :: reverseFront) cmd tailModels
        mapFirstRec [] Cmd.none modelList

    let mapFirstCmdIntent predicate updatef defaultIntent modelList =
        let rec mapFirstRec reverseFront cmd back =
            match back with
            | [] ->
                (*
                 * Conceptually, the correct value to return is
                 * reverseFront |> List.rev
                 * but this is the same as
                 * input
                 * so returning that instead.
                 *)
                modelList, cmd, defaultIntent
            | model :: tailModels ->
                if predicate model then
                    let (model, cmd, intent) = updatef model
                    (reverseFront |> List.rev) @ (model :: tailModels), cmd, intent
                else
                    mapFirstRec (model :: reverseFront) cmd tailModels
        mapFirstRec [] Cmd.none modelList

    let mapFirstIntent predicate updatef defaultIntent modelList =
        let rec mapFirstRec reverseFront back =
            match back with
            | [] ->
                (*
                 * Conceptually, the correct value to return is
                 * reverseFront |> List.rev
                 * but this is the same as
                 * input
                 * so returning that instead.
                 *)
                modelList, defaultIntent
            | model :: tailModels ->
                if predicate model then
                    let (model, intent) = updatef model
                    (reverseFront |> List.rev) @ (model :: tailModels), intent
                else
                    mapFirstRec (model :: reverseFront) tailModels
        mapFirstRec [] modelList


/// For Elmish.WPF
module Reflection =
    open System.Reflection

    let bindingProperties (bindingsType: System.Type) =
        bindingsType.GetProperties(BindingFlags.GetProperty ||| BindingFlags.Instance ||| BindingFlags.Public)
        |> Array.filter (fun pi -> pi.ReflectedType = bindingsType)

    let bindings<'Binding> (bindingsTypeInstance: obj) (bindingsTypeInstanceProperties: System.Reflection.PropertyInfo array) =
        bindingsTypeInstanceProperties
        |> Array.map (fun pi ->
            pi.GetValue(bindingsTypeInstance) :?> 'Binding
        )
        |> Array.toList