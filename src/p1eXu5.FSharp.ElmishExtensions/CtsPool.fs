namespace p1eXu5.FSharp.ElmishExtensions

open System.Threading
open System
open System.Buffers

type Cts =
    {
        Token: unit -> CancellationToken
        Cancel: unit -> unit
        Free: unit -> unit
    }
    interface IDisposable with
        member this.Dispose (): unit = 
            this.Free()

type internal CtsPool =
    {
        Size: unit -> int
        InUse: unit -> int
        GetCts: unit -> Cts
        Dispose: unit -> unit
    }
    interface IDisposable with
        member this.Dispose (): unit = 
            this.Dispose()

module internal CtsPool =

    type private State =
        {
            Pool: CancellationTokenSource array
            Using: byte array
            Cancelled: byte array
        }

    module private State =
        let private initSize = 8

        let init () =

            let pool = ArrayPool<CancellationTokenSource>.Shared.Rent(initSize)
            Array.fill pool 0 pool.Length Unchecked.defaultof<_>

            let usingFlags = ArrayPool<byte>.Shared.Rent(initSize)
            Array.fill usingFlags 0 usingFlags.Length 0uy

            let cancelled = ArrayPool<byte>.Shared.Rent(initSize)
            Array.fill cancelled 0 cancelled.Length 0uy

            if pool.Length <> usingFlags.Length || usingFlags.Length <> cancelled.Length then
                raise (InvalidOperationException("CtsPool internal arrays have different length"))

            {
                Pool = pool
                Using = usingFlags
                Cancelled = cancelled
            }

        let inUse state =
            state.Using |> Array.sum |> int

        let tryIncrease (state: State) =
            if (state |> inUse) = state.Pool.Length then
                let pool' = ArrayPool<CancellationTokenSource>.Shared.Rent(state.Pool.Length + 8)
                Array.Copy(state.Pool, pool', state.Pool.Length)
                Array.fill pool' (state.Pool.Length - 1) (pool'.Length - state.Pool.Length) Unchecked.defaultof<_>
                ArrayPool<CancellationTokenSource>.Shared.Return(state.Pool)

                let using' = ArrayPool<byte>.Shared.Rent(state.Using.Length + 8)
                Array.Copy(state.Using, using', state.Using.Length)
                Array.fill using' (state.Using.Length - 1) (using'.Length - state.Using.Length) 0uy
                ArrayPool<byte>.Shared.Return(state.Using)

                let cancelled' = ArrayPool<byte>.Shared.Rent(state.Cancelled.Length + 8)
                Array.Copy(state.Cancelled, cancelled', state.Cancelled.Length)
                Array.fill cancelled' (state.Cancelled.Length - 1) (cancelled'.Length - state.Cancelled.Length) 0uy
                ArrayPool<byte>.Shared.Return(state.Cancelled)

                if pool'.Length <> using'.Length || using'.Length <> cancelled'.Length then
                    raise (InvalidOperationException("CtsPool internal arrays have different length"))

                { state with
                    Pool = pool'
                    Using = using'
                    Cancelled = cancelled'
                }
            else
                state

        let getCtsIndex state =
            let ind = state.Using |> Array.findIndex ((=) 0uy)
            if state.Pool[ind] = null then
                state.Pool[ind] <- new CancellationTokenSource()
            state.Using[ind] <- 1uy
            (state, ind)

        let tryGetToken index state =
            if state.Using[index] = 1uy then
                state.Pool[index].Token |> Some
            else
                None

        let cancel index state =
            if state.Using[index] = 1uy && state.Cancelled[index] = 0uy then
                state.Pool[index].Cancel()
                state.Cancelled[index] <- 1uy
            else
                ()

        let free index state =
            if state.Using[index] = 1uy then
                if state.Cancelled[index] = 1uy then
                    state.Cancelled[index] <- 0uy
                    state.Pool[index].Dispose()
                    state.Pool[index] <- Unchecked.defaultof<_>

                elif state.Pool[index].TryReset() = false then
                    state.Pool[index].Dispose()
                    state.Pool[index] <- Unchecked.defaultof<_>

                state.Using[index] <- 0uy
            else
                ()

        let freeAll state =
            state.Pool
            |> Array.filter ((<>) null)
            |> Array.iter (fun cts -> cts.Dispose())

            ArrayPool<CancellationTokenSource>.Shared.Return(state.Pool)
            ArrayPool<byte>.Shared.Return(state.Using)
            ArrayPool<byte>.Shared.Return(state.Cancelled)

            {
                Pool = [||]
                Using = [||]
                Cancelled = [||]
            }

    type private Msg =
        | GetPoolSize of AsyncReplyChannel<int>
        | GetCtsInUse of AsyncReplyChannel<int>
        | GetCts of AsyncReplyChannel<int>
        | GetToken of AsyncReplyChannel<CancellationToken option> * int
        | Cancel of AsyncReplyChannel<unit> * int
        | Free of AsyncReplyChannel<unit> * int
        | FreeAll

    let private body = (fun (inbox: MailboxProcessor<_>) ->
        let rec loop state =
            async {
                let! msg = inbox.Receive ()
                match msg with
                | Msg.GetCtsInUse reply ->
                    reply.Reply (state |> State.inUse)
                    return! loop state

                | Msg.GetPoolSize reply ->
                    reply.Reply state.Pool.Length
                    return! loop state

                | Msg.GetCts reply ->
                    return! loop (
                        state
                        |> State.tryIncrease
                        |> State.getCtsIndex
                        |> fun (s, i) ->
                            reply.Reply i
                            s
                    )

                | Msg.GetToken (reply, ind) ->
                    state
                    |> State.tryGetToken ind
                    |> reply.Reply

                    return! loop state

                | Msg.Cancel (reply, ind) ->
                    state |> State.cancel ind
                    reply.Reply ()
                    return! loop state

                | Msg.Free (reply, ind) ->
                    state |> State.free ind
                    reply.Reply()
                    return! loop state

                | Msg.FreeAll ->
                    state |> State.freeAll |> ignore
                    return () // terminate!
            }

        loop (State.init ())
    )

    let init () : CtsPool =
        let agent = MailboxProcessor.Start(body)
        {
            Dispose = fun () ->
                agent.Post Msg.FreeAll
                agent.Dispose()
            Size = fun () -> agent.PostAndReply Msg.GetPoolSize
            InUse = fun () -> agent.PostAndReply Msg.GetCtsInUse
            GetCts = fun () ->
                let ind = agent.PostAndReply Msg.GetCts
                {
                    Token = fun () ->
                        match agent.PostAndReply (fun reply -> Msg.GetToken (reply, ind)) with
                        | Some token -> token
                        | None -> raise (ObjectDisposedException("Cts already free."))
                    Cancel = fun () -> agent.PostAndReply (fun reply -> Msg.Cancel (reply, ind))
                    Free = fun () -> agent.PostAndReply (fun reply -> Msg.Free (reply, ind))
                }
        }
