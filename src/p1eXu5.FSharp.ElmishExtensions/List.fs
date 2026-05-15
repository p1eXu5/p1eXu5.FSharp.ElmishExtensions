namespace p1eXu5.FSharp.ElmishExtensions

/// <summary>
/// Provides Elmish extensions for list operations, including functions to map and update the first element matching a predicate.
/// </summary>
[<AutoOpen>]
module List =
    
    open Elmish

    /// <summary>
    /// Maps the first element in the list that satisfies the predicate.
    /// </summary>
    /// <param name="predicate">Function to test elements.</param>
    /// <param name="updatef">Function to apply to the first matching element.</param>
    /// <param name="modelList">The input list.</param>
    /// <returns>List with the first matching element updated, or the original list if no match found.</returns>
    let mapFirst<'model> (predicate: 'model -> bool) updatef modelList =
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
            | model :: tailModels ->
                if predicate model then
                    let model' = updatef model
                    (reverseFront |> List.rev) @ (model' :: tailModels)
                else
                    mapFirstRec (model :: reverseFront) tailModels
        mapFirstRec [] modelList

    /// <summary>
    /// Maps the first element in the list that satisfies the predicate.
    /// </summary>
    /// <param name="predicate">Function to test elements.</param>
    /// <param name="updatef">Function to apply to the first matching element.</param>
    /// <param name="modelList">The input list.</param>
    /// <returns>List with the first matching element updated, or the original list if no match found.</returns>
    let tryMapFirst<'model> (predicate: 'model -> bool) updatef modelList =
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
                modelList, None
            | model :: tailModels ->
                if predicate model then
                    let model' = updatef model
                    (reverseFront |> List.rev) @ (model' :: tailModels), model' |> Some
                else
                    mapFirstRec (model :: reverseFront) tailModels
        mapFirstRec [] modelList

    /// <summary>
    /// Maps the first element in the list that satisfies the predicate and returns a command.
    /// </summary>
    /// <param name="predicate">Function to test elements.</param>
    /// <param name="updatef">Function to apply to the first matching element, returning updated model and command.</param>
    /// <param name="modelList">The input list.</param>
    /// <returns>Tuple of updated list and command.</returns>
    let mapFirstCmd<'model,'msg> (predicate: 'model -> bool) (updatef: 'model -> 'model * Cmd<'msg>) modelList =
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
                    let (model', cmd) = updatef model
                    (reverseFront |> List.rev) @ (model' :: tailModels), cmd
                else
                    mapFirstRec (model :: reverseFront) cmd tailModels
        mapFirstRec [] Cmd.none modelList

    /// <summary>
    /// Maps the first element in the list that satisfies the predicate and returns a command.
    /// </summary>
    /// <param name="predicate">Function to test elements.</param>
    /// <param name="updatef">Function to apply to the first matching element, returning updated model and command.</param>
    /// <param name="modelList">The input list.</param>
    /// <returns>Tuple of updated list and command.</returns>
    let tryMapFirstCmd<'model,'msg> (predicate: 'model -> bool) (updatef: 'model -> 'model * Cmd<'msg>) modelList =
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
                modelList, cmd, None
            | model :: tailModels ->
                if predicate model then
                    let (model', cmd) = updatef model
                    (reverseFront |> List.rev) @ (model' :: tailModels), cmd, model' |> Some
                else
                    mapFirstRec (model :: reverseFront) cmd tailModels
        mapFirstRec [] Cmd.none modelList

    /// <summary>
    /// Maps the first element in the list that satisfies the predicate and returns a command and intent.
    /// </summary>
    /// <param name="predicate">Function to test elements.</param>
    /// <param name="updatef">Function to apply to the first matching element, returning updated model, command, and intent.</param>
    /// <param name="defaultIntent">The default intent to return if no match found.</param>
    /// <param name="modelList">The input list.</param>
    /// <returns>Tuple of updated list, command, and intent.</returns>
    let mapFirstCmdIntent<'model,'msg,'intent> (predicate: 'model -> bool) (updatef: 'model -> 'model * Cmd<'msg> * 'intent) defaultIntent modelList =
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

    /// <summary>
    /// Maps the first element in the list that satisfies the predicate and returns a command and intent.
    /// </summary>
    /// <param name="predicate">Function to test elements.</param>
    /// <param name="updatef">Function to apply to the first matching element, returning updated model, command, and intent.</param>
    /// <param name="defaultIntent">The default intent to return if no match found.</param>
    /// <param name="modelList">The input list.</param>
    /// <returns>Tuple of updated list, command, and intent.</returns>
    let tryMapFirstCmdIntent<'model,'msg,'intent> (predicate: 'model -> bool) (updatef: 'model -> 'model * Cmd<'msg> * 'intent) defaultIntent modelList =
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
                modelList, cmd, defaultIntent, None
            | model :: tailModels ->
                if predicate model then
                    let (model', cmd, intent) = updatef model
                    (reverseFront |> List.rev) @ (model' :: tailModels), cmd, intent, model' |> Some
                else
                    mapFirstRec (model :: reverseFront) cmd tailModels
        mapFirstRec [] Cmd.none modelList

    /// <summary>
    /// Maps the first element in the list that satisfies the predicate and returns an intent.
    /// </summary>
    /// <param name="predicate">Function to test elements.</param>
    /// <param name="updatef">Function to apply to the first matching element, returning updated model and intent.</param>
    /// <param name="defaultIntent">The default intent to return if no match found.</param>
    /// <param name="modelList">The input list.</param>
    /// <returns>Tuple of updated list and intent.</returns>
    let mapFirstIntent<'model,'intent> (predicate: 'model -> bool) (updatef: 'model -> 'model * 'intent) defaultIntent modelList =
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

    /// <summary>
    /// Maps the first element in the list that satisfies the predicate and returns an intent.
    /// </summary>
    /// <param name="predicate">Function to test elements.</param>
    /// <param name="updatef">Function to apply to the first matching element, returning updated model and intent.</param>
    /// <param name="defaultIntent">The default intent to return if no match found.</param>
    /// <param name="modelList">The input list.</param>
    /// <returns>Tuple of updated list and intent.</returns>
    let mapFirstIntentf<'model,'subintent,'intent> (predicate: 'model -> bool) (updatef: 'model -> 'model * 'subintent) (intentf: 'model -> 'subintent -> 'intent) (defaultIntent:'intent) modelList =
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
                    let (model', intent) = updatef model
                    (reverseFront |> List.rev) @ (model' :: tailModels), intentf model' intent
                else
                    mapFirstRec (model :: reverseFront) tailModels
        mapFirstRec [] modelList

    /// <summary>
    /// Maps the first element in the list that satisfies the predicate and returns an intent.
    /// </summary>
    /// <param name="predicate">Function to test elements.</param>
    /// <param name="updatef">Function to apply to the first matching element, returning updated model and intent.</param>
    /// <param name="defaultIntent">The default intent to return if no match found.</param>
    /// <param name="modelList">The input list.</param>
    /// <returns>Tuple of updated list and intent.</returns>
    let tryMapFirstIntent<'model,'intent> (predicate: 'model -> bool) (updatef: 'model -> 'model * 'intent) defaultIntent modelList =
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
                modelList, defaultIntent, None
            | model :: tailModels ->
                if predicate model then
                    let (model', intent) = updatef model
                    (reverseFront |> List.rev) @ (model' :: tailModels), intent, model' |> Some
                else
                    mapFirstRec (model :: reverseFront) tailModels
        mapFirstRec [] modelList
