p1eXu5.FSharp.ElmishExtensions
==============================

[![NuGet](https://img.shields.io/badge/nuget-1.0.0-green)](https://www.nuget.org/packages/p1eXu5.FSharp.ElmishExtensions/1.0.0)

A comprehensive F# library providing helpful utilities and extensions for working with [Elmish](https://elmish.io/) and F# async workflows. This library includes powerful abstractions for managing asynchronous operations, cancellation token pooling, and deferred computations in WPF and other applications.

## Core Types

### Cts (Cancellation Token Source Wrapper)
A lightweight wrapper around `CancellationTokenSource` with pooling support for efficient memory management.

```fsharp
type Cts =
    {
        Token: unit -> CancellationToken
        Cancel: unit -> unit
        Free: unit -> unit
        Dispose: unit -> unit
    }
```

### CtsPool
Manages a reusable pool of `CancellationTokenSource` instances to reduce allocation overhead in high-frequency async operations.

### Operation<'TArg, 'TRes>
Represents a basic operation with start and finish states.

```fsharp
type Operation<'TArg, 'TRes> =
    | Start of 'TArg
    | Finish of 'TRes
```

### AsyncOperation<'Arg, 'Res>
An extension of `Operation` that tracks the `Cts` used for cancellation.

```fsharp
type AsyncOperation<'Arg, 'Res> =
    | Start of 'Arg
    | Finish of 'Res * operationId: Cts
```

### Deferred<'Retrieved>
Represents the state of a deferred computation without cancellation support.

```fsharp
type Deferred<'Retrieved> =
    | NotRequested
    | InProgress
    | Retrieved of 'Retrieved
```

### AsyncDeferred<'Retrieved>
A deferred computation with cancellation support through `Cts`.

```fsharp
type AsyncDeferred<'Retrieved> =
    | NotRequested
    | InProgress of Cts
    | Retrieved of 'Retrieved
```

### PreparingDeferred<'Preparing, 'Retrieved>
A deferred computation that tracks a preparing/initializing state and the final retrieved value.

```fsharp
type PreparingDeferred<'Preparing, 'Retrieved> =
    | Preparing of 'Preparing
    | InProgress of 'Preparing
    | Retrieved of 'Retrieved
```

### AsyncPreparingDeferred<'Preparing, 'Retrieved>
Similar to `PreparingDeferred` but with cancellation support.

```fsharp
type AsyncPreparingDeferred<'Preparing, 'Retrieved> =
    | Preparing of 'Preparing
    | InProgress of 'Preparing * Cts
    | Retrieved of 'Retrieved
```

### AsyncDeferredState & AsyncPreparingDeferredState
State-only variants of `AsyncDeferred` and `AsyncPreparingDeferred` that don't carry the retrieved value (useful when the value is stored elsewhere).

```fsharp
type AsyncDeferredState =
    | NotRequested
    | InProgress of Cts
    | Retrieved

type AsyncPreparingDeferredState<'Preparing> =
    | Preparing of 'Preparing
    | InProgress of 'Preparing * Cts
    | Retrieved
```

## Helper Functions

### AsyncOperation Helpers
Basic helper functions for constructing and working with `AsyncOperation` messages.

```fsharp
// Creates a Start message with arguments
AsyncOperation.start opMsgCtor args

// Creates a Start message with unit argument
AsyncOperation.startUnit opMsgCtor

// Creates a Finish constructor from a Cts
AsyncOperation.finish cts

// Creates a Finish message from a Cts
AsyncOperation.finishWithin msgCtor cts
```

### AsyncDeferred Helpers
Functions for managing async deferred values and cancellation.

```fsharp
// Cancels in-progress operation and starts a new one
// Returns None if already retrieved
AsyncDeferred.tryInProgressWithCancellation asyncDeferred

// Force starts a new in-progress operation, canceling any current one
AsyncDeferred.forceInProgressWithCancellation asyncDeferred

// Safely completes a deferred operation if Cts matches
AsyncDeferred.chooseRetrievedWithin retrievedValue cts asyncDeferred

// Safely completes with a Result type
AsyncDeferred.chooseRetrievedResultWithin asyncOperationResult cts asyncDeferred

// Extract retrieved value as option
AsyncDeferred.chooseRetrieved asyncDeferred

// Cancel and reset to NotRequested
AsyncDeferred.notRequestedWithCancellation asyncDeferred

// Force to Retrieved state
AsyncDeferred.forceRetrievedWithCancellation retrievedValue asyncDeferred

// Active pattern for safe Cts disposal and validation
AsyncDeferred.(|InProgressWithCts|_|) opCts asyncDeferred

// Extract value with exception if not retrieved
AsyncDeferred.retrievedValue asyncDeferred

// Convert to option
AsyncDeferred.toOption asyncDeferred

// Functor map
AsyncDeferred.map f asyncDeferred

// Map with model update pattern (returns value and Cmd)
AsyncDeferred.mapUpdate f asyncDeferred
```

### Model Helpers
Functional utilities for working with model fields.

```fsharp
// Map over a model field using getter and setter
Model.map get set f model
```

### List Helpers
Utilities for updating specific elements in lists.

```fsharp
// Update first element matching predicate
List.mapFirst predicate updatef modelList

// Update first element with Cmd result
List.mapFirstCmd predicate updatef modelList

// Update first element with Cmd and Intent
List.mapFirstCmdIntent predicate updatef defaultIntent modelList

// Update first element with Intent
List.mapFirstIntent predicate updatef defaultIntent modelList
```

### General Helpers
Simple utility functions.

```fsharp
// Return model with Cmd.none
withCmdNone model

// Flip argument order for a function
flip f b a  // equivalent to f a b
```

## Examples

### AsyncDeferred State Management

```fsharp
type StatisticListModel =
    {
        StatisticList: AsyncDeferred<StatisticModel list>
        StartDate: DateOnly
        EndDate: DateOnly
    }

module StatisticListModel =
    let period (model: StatisticListModel) =
        (model.StartDate, model.EndDate) ||> DateOnlyPeriod.create

     let withStatisticList deff (model: StatisticListModel) =
        { model with StatisticList = deff }

type Msg =
    | LoadStatisticList of AsyncOperation<unit, Result<DailyStatistic list, string>>

module MsgWith =

    let (|``Start of LoadStatisticList``|_|) (model: StatisticListModel) (msg: Msg) =
        match msg with
        | Msg.LoadStatisticList (AsyncOperation.Start _) ->
            model.StatisticList |> AsyncDeferred.forceInProgressWithCancellation |> Some
        | _ -> None

    let (|``Finish of LoadStatisticList``|_|) (model: StatisticListModel) (msg: Msg) =
        match msg with
        | Msg.LoadStatisticList (AsyncOperation.Finish (res, cts)) ->
            model.StatisticList
            |> AsyncDeferred.chooseRetrievedResultWithin res cts
            |> Option.map (
                Result.map (fun (_, res) ->
                    res
                    |> List.map StatisticModel.init
                    |> AsyncDeferred.Retrieved
                )
            )
        | _ -> None

module Program =

    let update eventStore errorMessageQueue msg model =
        match msg with
        | MsgWith.``Start of LoadStatisticList`` model (deff, cts) ->
            let period = model |> period

            model |> withStatisticList deff
            , Cmd.OfTask.perform
                eventStore.ProjectStatisticList
                (period, cts.Token)
                (AsyncOperation.finishWithin Msg.LoadStatisticList cts)
            , Intent.None

        | MsgWith.``Finish of LoadStatisticList`` model res ->
            match res with
            | Ok deff ->
                model |> withStatisticList deff
                , Cmd.none
                , Intent.None
            | Error err ->
                do errorMessageQueue.EnqueueError err
                logger.LogError(err)
                model |> withStatisticList AsyncDeferred.NotRequested
                , Cmd.none
                , Intent.None

module Bindings =

    let bindings () =
        [
            LoadStatisticListCommand
                |> Binding.cmd (AsyncOperation.startUnit Msg.LoadStatisticList)
        ]
```