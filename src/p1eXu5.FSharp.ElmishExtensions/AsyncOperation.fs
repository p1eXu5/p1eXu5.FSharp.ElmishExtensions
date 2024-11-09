namespace p1eXu5.FSharp.ElmishExtensions

type AsyncOperation<'Arg, 'Res> =
    | Start of 'Arg
    | Finish of 'Res * operationId: Cts

[<RequireQualifiedAccess>]
module AsyncOperation =

    let start opMsgCtor args = AsyncOperation.Start args |> opMsgCtor

    let startWith args opMsgCtor = AsyncOperation.Start args |> opMsgCtor

    let startUnit opMsgCtor = AsyncOperation.Start () |> opMsgCtor

    let finish cts =
        fun res -> AsyncOperation.Finish (res, cts)

    let finishWithin msgCtor cts =
        fun res -> AsyncOperation.Finish (res, cts) |> msgCtor

