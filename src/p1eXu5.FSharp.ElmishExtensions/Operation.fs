﻿namespace p1eXu5.FSharp.ElmishExtensions

/// <summary>
/// see <see href="https://zaid-ajaj.github.io/the-elmish-book/#/chapters/commands/async-state"/>
/// </summary>
type Operation<'TArg, 'TRes> =
    | Start of 'TArg
    | Finish of 'TRes


