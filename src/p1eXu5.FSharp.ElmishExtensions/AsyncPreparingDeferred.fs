namespace p1eXu5.FSharp.ElmishExtensions

[<RequireQualifiedAccess>]
type AsyncPreparingDeferred<'Preparing, 'Retrieved> =
    | Preparing of 'Preparing
    | InProgress of 'Preparing * Cts
    | Retrieved of 'Retrieved

[<RequireQualifiedAccess>]
module AsyncPreparingDeferred =

    open System

    /// If asyncDeferred is InProgress then cancels it. If asyncDeferred is Retrieved then
    /// returns None
    let tryInProgressWithCancellation<'Preparing, 'Retrieved>
        (asyncDeferred: AsyncPreparingDeferred<'Preparing, 'Retrieved>)
        : (AsyncPreparingDeferred<'Preparing, 'Retrieved> * 'Preparing * Cts) option
        =
        match asyncDeferred with
        | AsyncPreparingDeferred.InProgress (p, cts) ->
            // CancellationTokenSource is disposed in a Program module,
            // using LastInProgressWithCancellation active pattern below
            // last cts reference contains AsyncOperation Finish message
            cts.Dispose()
            let newCts = ctsPool.GetCts ()
            (AsyncPreparingDeferred.InProgress (p, newCts), p, newCts) |> Some
        | AsyncPreparingDeferred.Preparing p ->
            let newCts = ctsPool.GetCts ()
            (AsyncPreparingDeferred.InProgress (p, newCts), p, newCts) |> Some
        | _ ->
            None

    /// If asyncDeferred is InProgress then cancels it.
    let forceInProgressWithCancellationf<'Preparing, 'Retrieved>
        (preparingf: 'Retrieved -> 'Preparing)
        (asyncDeferred: AsyncPreparingDeferred<'Preparing, 'Retrieved>)
        : (AsyncPreparingDeferred<'Preparing, 'Retrieved> * 'Preparing * Cts)
        =
        let newCts = ctsPool.GetCts ()
        match asyncDeferred with
        | AsyncPreparingDeferred.InProgress (p, cts) ->
            // CancellationTokenSource is disposed in a Program module,
            // using LastInProgressWithCancellation active pattern below
            // last cts reference contains AsyncOperation Finish message
            cts.Dispose()
            (AsyncPreparingDeferred.InProgress (p, newCts), p, newCts)
        | AsyncPreparingDeferred.Preparing p ->
            (AsyncPreparingDeferred.InProgress (p, newCts), p, newCts)
        | AsyncPreparingDeferred.Retrieved r ->
            let p = r |> preparingf
            (AsyncPreparingDeferred.InProgress (p, newCts), p, newCts)

    /// If asyncDeferred is InProgress then cancels it.
    let forceInProgressWithCancellation<'Preparing, 'Retrieved>
        (prepearing)
        (asyncDeferred: AsyncPreparingDeferred<'Preparing, 'Retrieved>)
        : (AsyncPreparingDeferred<'Preparing, 'Retrieved> * 'Preparing * Cts)
        =
        let newCts = ctsPool.GetCts ()
        match asyncDeferred with
        | AsyncPreparingDeferred.InProgress (_, cts) ->
            // CancellationTokenSource is disposed in a chooseRetrieved below
            // last cts reference contains AsyncOperation Finish message
            cts.Dispose()
            (AsyncPreparingDeferred.InProgress (prepearing, newCts), prepearing, newCts)
        | AsyncPreparingDeferred.Preparing _ ->
            (AsyncPreparingDeferred.InProgress (prepearing, newCts), prepearing, newCts)
        | AsyncPreparingDeferred.Retrieved _ ->
            (AsyncPreparingDeferred.InProgress (prepearing, newCts), prepearing, newCts)

    /// Is operation cts is equal to in progress deferred cts then return Some with cts disposing
    /// or returns None with disposing operation cts.
    let chooseRetrieved<'Preparing,'Args,'Retrieved>
        (retrievedValue: 'Retrieved)
        (asyncOperationCts: Cts)
        (asyncDeferred: AsyncPreparingDeferred<'Preparing, 'Retrieved>)
        : (AsyncPreparingDeferred<'Preparing, 'Retrieved> * 'Retrieved) option
        =
        match asyncDeferred with
        | AsyncPreparingDeferred.InProgress (_, cts) when obj.ReferenceEquals(cts, asyncOperationCts) ->
            cts.Free()
            (AsyncPreparingDeferred.Retrieved retrievedValue, retrievedValue) |> Some
        | _ -> // finished operation that we do not expect
            asyncOperationCts.Free() // just dispose operation cts
            None

    /// Is operation cts is equal to in progress deferred cts then return Some with cts disposing.
    /// Or returns None with disposing operation cts.
    let chooseRetrievedResult<'Preparing,'Args,'Retrieved,'Error>
        (asyncOperationResult: Result<'Retrieved,'Error>)
        (asyncOperationCts: Cts)
        (asyncDeferred: AsyncPreparingDeferred<'Preparing, 'Retrieved>)
        : (Result<AsyncPreparingDeferred<'Preparing, 'Retrieved> * 'Retrieved * 'Preparing, 'Error>) option
        =
        match asyncDeferred with
        | AsyncPreparingDeferred.InProgress (preparingModel, cts) when obj.ReferenceEquals(cts, asyncOperationCts) ->
            cts.Free()
            asyncOperationResult
            |> Result.map (fun retrievedValue -> (AsyncPreparingDeferred.Retrieved retrievedValue, retrievedValue, preparingModel))
            |> Some
        | _ -> // finished operation that we do not expect
            asyncOperationCts.Free() // just dispose operation cts
            None

    /// If asyncDeferred is InProgress then cancels it and returns NotRequested.
    let notRequestedWithCancellation<'Preparing, 'Retrieved>
        (preparingf: 'Retrieved -> 'Preparing)
        (asyncDeferred: AsyncPreparingDeferred<'Preparing, 'Retrieved>)
        =
        match asyncDeferred with
        | AsyncPreparingDeferred.InProgress (p, cts) ->
            // CancellationTokenSource is disposed in a Program module,
            // using LastInProgressWithCancellation active pattern below
            // last cts reference contains AsyncOperation Finish message
            cts.Cancel()
            (AsyncPreparingDeferred.Preparing p)
        | AsyncPreparingDeferred.Retrieved r ->
            (AsyncPreparingDeferred.Preparing (r |> preparingf))
        | _ -> asyncDeferred

    /// Invokes opCts.Dispose(), returns Some () if opCts is equal to AsyncDeferred.InProgress cts.
    let (|InProgressWithCts|_|) (opCts: Cts) (asyncDeferred: AsyncPreparingDeferred<_,_>) =
        opCts.Free()
        match asyncDeferred with
        | AsyncPreparingDeferred.InProgress (_, defCts) when obj.ReferenceEquals(opCts, defCts) ->
            Some ()
        | _ -> None

    /// <summary>
    /// Tries to extract AsyncDeferred.Retrieved case value.
    /// </summary>
    /// <exception cref="InvalidOperationException">Value has not been retrieved.</exception>
    let retrievedValue = function
        | AsyncPreparingDeferred.Retrieved v -> v
        | _ -> raise (InvalidOperationException("Value has not been retrieved."))

    /// <summary>
    /// Tries to extract AsyncDeferred.Retrieved case value.
    /// </summary>
    /// <exception cref="InvalidOperationException">Value has not been retrieved.</exception>
    let preparingValue = function
        | AsyncPreparingDeferred.Preparing v
        | AsyncPreparingDeferred.InProgress (v, _) -> v
        | _ -> raise (InvalidOperationException("Value has been retrieved."))

    let disableRetrieved _ =
        raise (InvalidOperationException("Could not transpose from retrieved state."))

    let apply f (asyncPreparingDeffered: AsyncPreparingDeferred<'a,'a>) : AsyncPreparingDeferred<'b,'b> =
        match asyncPreparingDeffered with
        | AsyncPreparingDeferred.Preparing v -> v |> f |> AsyncPreparingDeferred.Preparing
        | AsyncPreparingDeferred.InProgress (v, cts) -> (v |> f, cts) |> AsyncPreparingDeferred.InProgress
        | AsyncPreparingDeferred.Retrieved v -> v |> f |> AsyncPreparingDeferred.Retrieved

    module List =

        let tryFind predicate (deff: AsyncPreparingDeferred<'a list, 'a list>) =
            match deff with
            | AsyncPreparingDeferred.Preparing l
            | AsyncPreparingDeferred.InProgress (l,_)
            | AsyncPreparingDeferred.Retrieved l -> l |> List.tryFind predicate

        let unwrap (deff: AsyncPreparingDeferred<'a list, 'a list>) =
            match deff with
            | AsyncPreparingDeferred.Preparing l
            | AsyncPreparingDeferred.InProgress (l,_)
            | AsyncPreparingDeferred.Retrieved l -> l


