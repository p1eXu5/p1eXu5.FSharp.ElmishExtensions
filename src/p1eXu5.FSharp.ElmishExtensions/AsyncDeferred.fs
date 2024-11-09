namespace p1eXu5.FSharp.ElmishExtensions

[<RequireQualifiedAccess>]
type AsyncDeferred<'Retrieved> =
    | NotRequested
    | InProgress of Cts
    | Retrieved of 'Retrieved


[<RequireQualifiedAccess>]
module AsyncDeferred =

    open System

    /// If asyncDeferred is InProgress then cancels it. Returns None if AsyncDeferred is already retrieved.
    let tryInProgressWithCancellation<'Retrieved>
        (asyncDeferred: AsyncDeferred<'Retrieved>)
        : (AsyncDeferred<'Retrieved> * Cts) option
        =
        match asyncDeferred with
        | AsyncDeferred.InProgress (cts) ->
            // CancellationTokenSource is disposed in a Program module,
            // using LastInProgressWithCancellation active pattern below
            // last cts reference contains AsyncOperation Finish message
            cts.Cancel()
            let newCts = ctsPool.GetCts ()
            (AsyncDeferred.InProgress (newCts), newCts) |> Some
        | AsyncDeferred.NotRequested ->
            let newCts = ctsPool.GetCts ()
            (AsyncDeferred.InProgress (newCts), newCts) |> Some
        | _ ->
            None

    /// If asyncDeferred is InProgress then cancels it.
    let forceInProgressWithCancellation<'Retrieved>
        (asyncDeferred: AsyncDeferred<'Retrieved>)
        : (AsyncDeferred<'Retrieved> * Cts)
        =
        let newCts = ctsPool.GetCts ()
        match asyncDeferred with
        | AsyncDeferred.InProgress (cts) ->
            // CancellationTokenSource is disposed in a Program module,
            // using LastInProgressWithCancellation active pattern below
            // last cts reference contains AsyncOperation Finish message
            cts.Cancel()
            (AsyncDeferred.InProgress (newCts), newCts)
        | AsyncDeferred.NotRequested ->
            (AsyncDeferred.InProgress (newCts), newCts)
        | AsyncDeferred.Retrieved r ->
            (AsyncDeferred.InProgress (newCts), newCts)

    /// Is operation cts is equal to in progress deferred cts then return Some with cts disposing
    /// or returns None with disposing operation cts.
    let chooseRetrievedWithin<'Preparing,'Args,'Retrieved>
        (retrievedValue: 'Retrieved)
        (asyncOperationCts: Cts)
        (asyncDeferred: AsyncDeferred<'Retrieved>)
        : (AsyncDeferred<'Retrieved> * 'Retrieved) option
        =
        match asyncDeferred with
        | AsyncDeferred.InProgress (cts) when obj.ReferenceEquals(cts, asyncOperationCts) ->
            cts.Free()
            (AsyncDeferred.Retrieved retrievedValue, retrievedValue) |> Some
        | _ -> // finished operation that we do not expect
            asyncOperationCts.Free() // just dispose operation cts
            None

    /// Is operation cts is equal to in progress deferred cts then return Some with cts disposing
    /// or returns None with disposing operation cts.
    let chooseRetrievedResultWithin<'Args,'Res,'Retrieved,'Error>
        (asyncOperationResult: Result<'Res,'Error>)
        (asyncOperationCts: Cts)
        (asyncDeferred: AsyncDeferred<'Retrieved>)
        : (Result<AsyncDeferred<'Res> * 'Res,'Error>) option
        =
        match asyncDeferred with
        | AsyncDeferred.InProgress (cts) when obj.ReferenceEquals(cts, asyncOperationCts) ->
            cts.Free()
            asyncOperationResult
            |> Result.map (fun retrievedValue -> (AsyncDeferred.Retrieved retrievedValue, retrievedValue))
            |> Some
        | _ -> // finished operation that we do not expect
            asyncOperationCts.Free() // just dispose operation cts
            None

    let chooseRetrieved<'Preparing,'Args,'Retrieved> (asyncDeferred: AsyncDeferred<'Retrieved>) : 'Retrieved option =
        match asyncDeferred with
        | AsyncDeferred.Retrieved v -> v |> Some
        | _ -> None

    /// If asyncDeferred is InProgress then cancels it and returns NotRequested.
    let notRequestedWithCancellation<'Retrieved>
        (asyncDeferred: AsyncDeferred<'Retrieved>) =
        match asyncDeferred with
        | AsyncDeferred.InProgress (cts) ->
            // CancellationTokenSource is disposed in a Program module,
            // using LastInProgressWithCancellation active pattern below
            // last cts reference contains AsyncOperation Finish message
            cts.Cancel()
            (AsyncDeferred.NotRequested)
        | AsyncDeferred.Retrieved _ ->
            (AsyncDeferred.NotRequested)
        | _ -> asyncDeferred

    /// If asyncDeferred is InProgress then cancels it.
    let forceRetrievedWithCancellation<'Retrieved>
        (retrievedValue: 'Retrieved)
        (asyncDeferred: AsyncDeferred<'Retrieved>)
        : AsyncDeferred<'Retrieved>
        =
        match asyncDeferred with
        | AsyncDeferred.InProgress (cts) ->
            // CancellationTokenSource is disposed in a Program module,
            // using LastInProgressWithCancellation active pattern below
            // last cts reference contains AsyncOperation Finish message
            cts.Cancel()
            (AsyncDeferred.Retrieved retrievedValue)
        | _ ->
            (AsyncDeferred.Retrieved retrievedValue)


    /// Invokes opCts.Dispose(), returns Some () if opCts is equal to AsyncDeferred.InProgress cts.
    let (|InProgressWithCts|_|) (opCts: Cts) (asyncDeferred: AsyncDeferred<_>) =
        opCts.Free()
        match asyncDeferred with
        | AsyncDeferred.InProgress defCts when obj.ReferenceEquals(opCts, defCts) ->
            Some ()
        | _ -> None

    /// <summary>
    /// Tries to extract AsyncDeferred.Retrieved case value.
    /// </summary>
    /// <exception cref="InvalidOperationException">Value has not been retrieved.</exception>
    let retrievedValue = function
        | AsyncDeferred.Retrieved v -> v
        | _ -> raise (InvalidOperationException("Value has not been retrieved."))

    let toOption<'Retrieved> (asyncDeferred: AsyncDeferred<'Retrieved>) : 'Retrieved option =
        asyncDeferred
        |> function
            | AsyncDeferred.Retrieved v -> v |> Some
            | _ -> None

    let map<'a, 'b> (f: 'a -> 'b) = function
        | AsyncDeferred.Retrieved v -> v |> f |> AsyncDeferred.Retrieved
        | AsyncDeferred.NotRequested -> AsyncDeferred.NotRequested
        | AsyncDeferred.InProgress cts -> AsyncDeferred.InProgress cts

    let mapUpdate f = function
        | AsyncDeferred.Retrieved retrieved ->
            let (retrieved', cmd) = retrieved |> f
            (retrieved' |> AsyncDeferred.Retrieved), cmd
        | deff -> deff, Elmish.Cmd.none




