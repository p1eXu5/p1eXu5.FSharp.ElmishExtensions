namespace p1eXu5.FSharp.ElmishExtensions

/// Provides reflection utilities for working with Elmish.WPF bindings.
/// 
/// This module contains helper functions to discover and extract binding properties
/// from types and instances using reflection.
module Reflection =
    open System.Reflection

    /// Gets all public instance properties defined directly on the specified type.
    /// 
    /// Filters properties to only include those declared on the binding type itself,
    /// excluding inherited properties.
    let bindingProperties (bindingsType: System.Type) =
        bindingsType.GetProperties(BindingFlags.GetProperty ||| BindingFlags.Instance ||| BindingFlags.Public)
        |> Array.filter (fun pi -> pi.ReflectedType = bindingsType)

    /// Extracts binding values from an instance by reflecting over its properties.
    /// 
    /// Maps each property to its corresponding binding value, casting to the specified type.
    let bindings<'Binding> (bindingsTypeInstance: obj) (bindingsTypeInstanceProperties: System.Reflection.PropertyInfo array) =
        bindingsTypeInstanceProperties
        |> Array.map (fun pi ->
            pi.GetValue(bindingsTypeInstance) :?> 'Binding
        )
        |> Array.toList
