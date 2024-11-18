(*
   Example of usage:

        ===================================

        open p1eXu5.FSharp.ElmishExtensions
        
        type Model =
            {
                SearchState: AsyncPreparingDeferredState<string>
            }
        
        module Model =
        
            type Msg =
                | Search of AsyncOperation<string, SearchResult>
        
            module MsgWith =
        
                let (|``Start of Search``|_|) (model: Model) = function
                    | Msg.Search (AsyncOperation.Start search) ->
                        model.SearchState |> AsyncPreparingDeferredState.forceInProgressWithCancellation search |> Some
                    | _ -> None
        
                let (|``Finish of Search``|_|) (model: Model) = function
                    | Msg.Search (AsyncOperation.Finish (res, cts)) ->
                        model.SearchState |> AsyncPreparingDeferredState.chooseRetrieved res cts
                    | _ -> None
        
            let init () =
                {
                    SearchState = AsyncPreparingDeferredState.NotRequested
                }

        ===================================
*)

namespace p1eXu5.FSharp.ElmishExtensions

[<RequireQualifiedAccess>]
type AsyncPreparingDeferredState<'Preparing> =
    | Preparing of 'Preparing
    | InProgress of 'Preparing * Cts
    | Retrieved of 'Preparing

[<RequireQualifiedAccess>]
module AsyncPreparingDeferredState =
    /// If asyncDeferred is InProgress then cancels it. Returns None if AsyncDeferred is already retrieved.
    let tryInProgressWithCancellation
        (prepearing)
        (asyncDeferred: AsyncPreparingDeferredState<_>)
        : (AsyncPreparingDeferredState<_> * Cts) option
        =
        match asyncDeferred with
        | AsyncPreparingDeferredState.InProgress (_, cts) ->
            // CancellationTokenSource is disposed in a chooseRetrieved below
            // last cts reference contains AsyncOperation Finish message
            cts.Dispose()
            let newCts = ctsPool.GetCts ()
            (AsyncPreparingDeferredState.InProgress (prepearing, newCts), newCts) |> Some
        | AsyncPreparingDeferredState.Preparing _ ->
            let newCts = ctsPool.GetCts ()
            (AsyncPreparingDeferredState.InProgress (prepearing, newCts), newCts) |> Some
        | _ ->
            None

    /// If asyncDeferred is InProgress then cancels it.
    let forceInProgressWithCancellation<'Preparing>
        (prepearing)
        (asyncDeferred: AsyncPreparingDeferredState<'Preparing>)
        : (AsyncPreparingDeferredState<'Preparing> * 'Preparing * Cts)
        =
        let newCts = ctsPool.GetCts ()
        match asyncDeferred with
        | AsyncPreparingDeferredState.InProgress (_, cts) ->
            // CancellationTokenSource is disposed in a chooseRetrieved below
            // last cts reference contains AsyncOperation Finish message
            cts.Dispose()
            (AsyncPreparingDeferredState.InProgress (prepearing, newCts), prepearing, newCts)
        | AsyncPreparingDeferredState.Preparing _ ->
            (AsyncPreparingDeferredState.InProgress (prepearing, newCts), prepearing, newCts)
        | AsyncPreparingDeferredState.Retrieved _ ->
            (AsyncPreparingDeferredState.InProgress (prepearing, newCts), prepearing, newCts)

    /// Is operation cts is equal to in progress deferred cts then return Some with cts disposing
    /// or returns None with disposing operation cts.
    let chooseRetrieved<'Preparing,'Res>
        (asyncOperationResult: 'Res)
        (asyncOperationCts: Cts)
        (asyncDeferred: AsyncPreparingDeferredState<'Preparing>)
        : (AsyncPreparingDeferredState<'Preparing> * 'Res) option
        =
        match asyncDeferred with
        | AsyncPreparingDeferredState.InProgress (prep, cts) when obj.ReferenceEquals(cts, asyncOperationCts) ->
            cts.Free()
            asyncOperationResult
            |> fun retrievedValue -> (AsyncPreparingDeferredState.Retrieved prep, retrievedValue)
            |> Some
        | _ -> // finished operation that we do not expect
            asyncOperationCts.Free() // just dispose operation cts
            None

    /// Is operation cts is equal to in progress deferred cts then return Some with cts disposing
    /// or returns None with disposing operation cts.
    let chooseRetrievedResult<'Preparing,'Res,'Error>
        (asyncOperationResult: Result<'Res,'Error>)
        (asyncOperationCts: Cts)
        (asyncDeferred: AsyncPreparingDeferredState<'Preparing>)
        : (Result<AsyncPreparingDeferredState<'Preparing> * 'Res,'Error>) option
        =
        match asyncDeferred with
        | AsyncPreparingDeferredState.InProgress (prep, cts) when obj.ReferenceEquals(cts, asyncOperationCts) ->
            cts.Free()
            asyncOperationResult
            |> Result.map (fun retrievedValue -> (AsyncPreparingDeferredState.Retrieved prep, retrievedValue))
            |> Some
        | _ -> // finished operation that we do not expect
            asyncOperationCts.Free() // just dispose operation cts
            None

    let tryCts (asyncDeferred: AsyncPreparingDeferredState<_>) =
        match asyncDeferred with
        | AsyncPreparingDeferredState.InProgress (_, cts) -> cts |> Some
        | _ -> None


