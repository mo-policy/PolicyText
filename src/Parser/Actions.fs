// Copyright (c) Mobile Ownership, mobileownership.org.  All Rights Reserved.  See LICENSE.txt in the project root for license information.

namespace MobileOwnership.PolicyText

open FSharp.Text.Parsing

[<RequireQualifiedAccess>]
type Value =
    | Object        of (string * Value) list
    | Array         of Value list
    | String        of string
    | Integer       of int64
    | Double        of double
    | Boolean       of bool
    | Null

module Actions = 
    let termObject (kind: int) (parseState: IParseState)  : Value =
        match kind with
        | 1 -> 
            Value.Object([])
        | 2 ->
            let members = parseState.GetInput(2) :?> (string * Value) list
            Value.Object(List.rev members)
        | _ ->
            failwith "unexpected kind"

    let termArray (kind: int) (parseState: IParseState)  : Value =
        match kind with
        | 1 -> 
            Value.Array([])
        | 2 ->
            let elements = parseState.GetInput(2) :?> Value list
            Value.Array(List.rev elements)
        | _ ->
            failwith "unexpected kind"