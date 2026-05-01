namespace p1eXu5.FSharp.ElmishExtensions

[<AutoOpen>]
module Helpers =

    open Elmish

    /// <summary>
    /// Returns a model with Cmd.none as the command.
    /// </summary>
    let inline withCmdNone model = model, Cmd.none
    
    /// <summary>
    /// Flips the argument order of a function.
    /// </summary>
    let inline flip f b a = f a b
