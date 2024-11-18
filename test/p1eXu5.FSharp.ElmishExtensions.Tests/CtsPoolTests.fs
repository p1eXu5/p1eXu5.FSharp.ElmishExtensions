module p1eXu5.FSharp.ElmishExtensions.Tests

open NUnit.Framework
open Faqt
open Faqt.Operators
open System

let mutable private ctsPool = Unchecked.defaultof<_>

[<SetUp>]
let initPool () =
    ctsPool <- CtsPool.init ()

[<TearDown>]
let disposePool () =
    if box ctsPool <> null then
        ctsPool.Dispose()

[<Test>]
let GetCts_ByDeffault_ReturnsNotCancelledCts () =
    let cts = ctsPool.GetCts ()
    let token = cts.Token ()
    %token.IsCancellationRequested.Should().BeFalse()

[<Test>]
let GetCts_FreeAndGetAgain_ReturnsNotCancelledCts () =
    let cts = ctsPool.GetCts ()
    cts.Free()

    let cts = ctsPool.GetCts ()

    let token = cts.Token ()
    %token.IsCancellationRequested.Should().BeFalse()

[<Test>]
let GetCts_Cancel_ReturnsCancelledCts () =
    let cts = ctsPool.GetCts ()
    cts.Cancel()

    let token = cts.Token ()
    %token.IsCancellationRequested.Should().BeTrue()

[<Test>]
let GetCts_CancelFreeAndGetAgain_ReturnsNotCancelledCts () =
    let cts = ctsPool.GetCts ()
    cts.Cancel()
    cts.Free()

    let cts = ctsPool.GetCts ()
    let token = cts.Token ()
    %token.IsCancellationRequested.Should().BeFalse()

[<Test>]
let GetCts_FreeAndTryuGetToken_Throws () =
    let cts = ctsPool.GetCts ()
    cts.Free()

    let act = fun () -> cts.Token ()
    %act.Should().Throw<ObjectDisposedException, _>()


[<Test>]
let Size_ByDefault_EqualsToOrGreaterThan8 () =
    let size = ctsPool.Size ()
    %size.Should().BeGreaterThanOrEqualTo(8) 

[<Test>]
let InUse_ByDefault_EqualsTo0 () =
    let inUse = ctsPool.InUse ()
    %inUse.Should().Be(0) 

[<Test>]
let GetCts_RequestedCtsGreaterThanSize_IncreasesSize () =
    let size = ctsPool.Size ()
    let ctsArray =
        seq {
            for i in 1 .. size + 1 do
                ctsPool.GetCts ()
        }
        |> Seq.toList

    let size2 = ctsPool.Size ()
    %size2.Should().BeGreaterThan(size)

[<Test>]
let GetCts_PoolFilledOneFreeGetCts_DoesNotChangeSize () =
    let size = ctsPool.Size ()
    let ctsArray =
        seq {
            for i in 1 .. size do
                ctsPool.GetCts ()
        }
        |> Seq.toList

    ctsArray[0].Free ()

    let cts = ctsPool.GetCts ()

    let size2 = ctsPool.Size ()
    %size2.Should().Be(size)

[<Test>]
let GetCts_PoolFilledOneCancelled_DoesNotChangeSize () =
    let size = ctsPool.Size ()
    let ctsArray =
        seq {
            for i in 1 .. size do
                ctsPool.GetCts ()
        }
        |> Seq.toList

    ctsArray[0].Cancel ()

    let cts = ctsPool.GetCts ()

    let size2 = ctsPool.Size ()
    %size2.Should().Be(size)

