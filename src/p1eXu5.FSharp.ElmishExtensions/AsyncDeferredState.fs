namespace p1eXu5.FSharp.ElmishExtensions

[<RequireQualifiedAccess>]
type AsyncDeferredState =
    | NotRequested
    | InProgress of Cts
    | Retrieved

[<RequireQualifiedAccess>]
module AsyncDeferredState =
    /// If asyncDeferred is InProgress then cancels it. Returns None if AsyncDeferred is already retrieved.
    let tryInProgressWithCancellation
        (asyncDeferred: AsyncDeferredState)
        : (AsyncDeferredState * Cts) option
        =
        match asyncDeferred with
        | AsyncDeferredState.InProgress (cts) ->
            // CancellationTokenSource is disposed in a Program module,
            // using LastInProgressWithCancellation active pattern below
            // last cts reference contains AsyncOperation Finish message
            cts.Cancel()
            let newCts = ctsPool.GetCts ()
            (AsyncDeferredState.InProgress (newCts), newCts) |> Some
        | AsyncDeferredState.NotRequested ->
            let newCts = ctsPool.GetCts ()
            (AsyncDeferredState.InProgress (newCts), newCts) |> Some
        | _ ->
            None

    /// If asyncDeferred is InProgress then cancels it.
    let forceInProgressWithCancellation
        (asyncDeferred: AsyncDeferredState)
        : (AsyncDeferredState * Cts)
        =
        let newCts = ctsPool.GetCts ()
        match asyncDeferred with
        | AsyncDeferredState.InProgress (cts) ->
            // CancellationTokenSource is disposed in a Program module,
            // using LastInProgressWithCancellation active pattern below
            // last cts reference contains AsyncOperation Finish message
            cts.Cancel()
            (AsyncDeferredState.InProgress (newCts), newCts)
        | AsyncDeferredState.NotRequested ->
            (AsyncDeferredState.InProgress (newCts), newCts)
        | AsyncDeferredState.Retrieved ->
            (AsyncDeferredState.InProgress (newCts), newCts)

    /// Is operation cts is equal to in progress deferred cts then return Some with cts disposing
    /// or returns None with disposing operation cts.
    let chooseRetrieved<'Args,'Res,'Error>
        (asyncOperationResult: 'Res)
        (asyncOperationCts: Cts)
        (asyncDeferred: AsyncDeferredState)
        : (AsyncDeferredState * 'Res) option
        =
        match asyncDeferred with
        | AsyncDeferredState.InProgress (cts) when obj.ReferenceEquals(cts, asyncOperationCts) ->
            cts.Free()
            asyncOperationResult
            |> fun retrievedValue -> (AsyncDeferredState.Retrieved, retrievedValue)
            |> Some
        | _ -> // finished operation that we do not expect
            asyncOperationCts.Free() // just dispose operation cts
            None

    /// Is operation cts is equal to in progress deferred cts then return Some with cts disposing
    /// or returns None with disposing operation cts.
    let chooseRetrievedResultWithin<'Args,'Res,'Error>
        (asyncOperationResult: Result<'Res,'Error>)
        (asyncOperationCts: Cts)
        (asyncDeferred: AsyncDeferredState)
        : (Result<AsyncDeferredState * 'Res,'Error>) option
        =
        match asyncDeferred with
        | AsyncDeferredState.InProgress (cts) when obj.ReferenceEquals(cts, asyncOperationCts) ->
            cts.Free()
            asyncOperationResult
            |> Result.map (fun retrievedValue -> (AsyncDeferredState.Retrieved, retrievedValue))
            |> Some
        | _ -> // finished operation that we do not expect
            asyncOperationCts.Free() // just dispose operation cts
            None

    let tryCts (asyncDeferred: AsyncDeferredState) =
        match asyncDeferred with
        | AsyncDeferredState.InProgress (cts) -> cts |> Some
        | _ -> None


