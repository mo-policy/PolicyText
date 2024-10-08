// Copyright (c) Mobile Ownership, mobileownership.org.  All Rights Reserved.  See LICENSE.txt in the project root for license information.

namespace MobileOwnership.PolicyText

open System.IO
open System.Security.Cryptography
open System.Text.Json

[<RequireQualifiedAccess>]
type Value =
    | Hashed        of Value * byte[]
    | Map           of (string * Value) list
    | List          of Value list
    | String        of string
    | Integer       of int64
    | Double        of double
    | Boolean       of bool
    | Null
with
    member this.WriteJSON(s: Stream) =
        use w = new Utf8JsonWriter(s)
        this.WriteJSON(w)
        w.Flush()

    member this.WriteJSON(w: Utf8JsonWriter) =
        match this with
        | Value.Hashed(v, _) -> 
            v.WriteJSON(w)
        | Value.Map(elements) ->
            w.WriteStartObject()
            for (n, v) in elements do
                w.WritePropertyName(n)
                v.WriteJSON(w)
            w.WriteEndObject()
        | Value.List(items) ->
            w.WriteStartArray()
            for v in items do
                v.WriteJSON(w)
            w.WriteEndArray()
        | Value.String(v) ->
            w.WriteStringValue(v)
        | Value.Integer(v) ->
            w.WriteNumberValue(v)
        | Value.Double(v) ->
            w.WriteNumberValue(v)
        | Value.Boolean(v) ->
            w.WriteBooleanValue(v)
        | Value.Null ->
            w.WriteNullValue()

    static member private JsonStringBytes(s: string): byte array = 
        use ms = new MemoryStream()
        use sw = new StreamWriter(ms)
        sw.Write('\"')
        for c in s do
            match c with
            | '\\' -> sw.Write("\\\\")
            | '\"' -> sw.Write("\\\"")
            | '\b' -> sw.Write("\\b")
            | '\f' -> sw.Write("\\f")
            | '\n' -> sw.Write("\\n")
            | '\r' -> sw.Write("\\r")
            | '\t' -> sw.Write("\\t")
            | _ when System.Char.IsControl(c) ->
                let hhhh = int(c).ToString("x4")
                sw.Write("\\u")
                sw.Write(hhhh)
            | _ -> 
                sw.Write(c)
        sw.Write('\"')
        sw.Flush()
        ms.ToArray()

    static member GetHashed(term: Value) =
        match term with
        | Value.Hashed(_) -> term
        | Value.Map(elements) -> 
            use ms = new MemoryStream()
            use sw = new StreamWriter(ms)
            sw.Write('{')
            let mutable first = true
            for (n, v) in elements do
                if first then
                    first <- false
                else
                    sw.Write(',')
                match Value.GetHashed(v) with
                | Value.Hashed(_, h) ->
                    sw.Flush()
                    let ph = Value.JsonStringBytes(n)
                    for b in ph do
                        sw.BaseStream.WriteByte(b)
                    sw.BaseStream.Flush()
                    sw.Write(':')
                    sw.Flush()
                    for b in h do
                        sw.BaseStream.WriteByte(b)
                    sw.BaseStream.Flush()
                | _ ->
                    failwith "expected Value.Hashed"
            sw.Write('}')
            sw.Flush()
            sw.BaseStream.Flush()
            let data = ms.ToArray()
            let digest = SHA256.HashData(data)
            Value.Hashed(term, digest)
        | Value.List(items) -> 
            use ms = new MemoryStream()
            use sw = new StreamWriter(ms)
            sw.Write('[')
            let mutable first = true
            for v in items do
                if first then
                    first <- false
                else
                    sw.Write(',')
                match Value.GetHashed(v) with
                | Value.Hashed(_, h) ->
                    sw.Flush()
                    for b in h do
                        sw.BaseStream.WriteByte(b)
                | _ ->
                    failwith "expected Value.Hashed"
            sw.Write(']')
            sw.Flush()
            let data = ms.ToArray()
            let digest = SHA256.HashData(data)
            Value.Hashed(term, digest)
        | Value.String(v) -> 
            let data = Value.JsonStringBytes(v)
            let digest = SHA256.HashData(data)
            Value.Hashed(term, digest)
        | Value.Integer(v) ->
            use ms = new MemoryStream()
            use sw = new StreamWriter(ms)
            sw.Write(v)
            sw.Flush()
            let data = ms.ToArray()
            let digest = SHA256.HashData(data)
            Value.Hashed(term, digest)
        | Value.Double(v) -> 
            use ms = new MemoryStream()
            use sw = new StreamWriter(ms)
            let vs = v.ToString().ToLower()
            sw.Write(vs)
            sw.Flush()
            let data = ms.ToArray()
            let digest = SHA256.HashData(data)
            Value.Hashed(term, digest)
        | Value.Boolean(v) -> 
            use ms = new MemoryStream()
            use sw = new StreamWriter(ms)
            if v then
                sw.Write("true")
            else
                sw.Write("false")
            sw.Flush()
            let data = ms.ToArray()
            let digest = SHA256.HashData(data)
            Value.Hashed(term, digest)
        | Value.Null ->
            use ms = new MemoryStream()
            use sw = new StreamWriter(ms)
            sw.Write("null")
            sw.Flush()
            let data = ms.ToArray()
            let digest = SHA256.HashData(data)
            Value.Hashed(term, digest)
