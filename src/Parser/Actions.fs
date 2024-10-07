// Copyright (c) Mobile Ownership, mobileownership.org.  All Rights Reserved.  See LICENSE.txt in the project root for license information.

namespace MobileOwnership.PolicyText

open FSharp.Text.Parsing
open System.IO
open System.Security.Cryptography

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

module Actions = 

    let valueOrHashed (parseState: IParseState) (term: Value) : Value =
        let mutable ht = term
        if parseState.ParserLocalStore.ContainsKey("LexBuffer") then
            let lb = parseState.ParserLocalStore["LexBuffer"] :?> FSharp.Text.Lexing.LexBuffer<char>
            if lb.BufferLocalStore.ContainsKey("hashed") then
                if lb.BufferLocalStore["hashed"] = true then
                    ht <- Value.GetHashed(term)
        ht

    let termApplication (parseState: IParseState) : Value = 
        let appFunction = parseState.GetInput(1) :?> Value
        let appArg = parseState.GetInput(2) :?> Value
        let term = 
            Value.Map(
                [
                    ("$policy", Value.String("Application"));
                    ("function", appFunction);
                    ("arg", appArg);
                ]
            )
        valueOrHashed parseState term

    let termFunction (parseState: IParseState) : Value = 
        let funPattern = parseState.GetInput(2) :?> Value
        let funTerm = parseState.GetInput(4) :?> Value
        let term = 
            Value.Map(
                [
                    ("$policy", Value.String("Function"));
                    ("pattern", funPattern);
                    ("term", funTerm);
                ]
            )
        valueOrHashed parseState term

    let termAnnotation (parseState: IParseState) : Value = 
        let annotationTerm = parseState.GetInput(1) :?> Value
        let annotationType = parseState.GetInput(3) :?> Value
        let term = 
            Value.Map(
                [
                    ("$policy", Value.String("Annotation"));
                    ("term", annotationTerm);
                    ("type", annotationType);
                ]
            )
        valueOrHashed parseState term

    let termIf (kind: int) (parseState: IParseState) : Value = 
        let term = 
            match kind with
            | 1
            | 3 -> 
                let ifCondition = parseState.GetInput(2) :?> Value
                let ifThen = parseState.GetInput(4) :?> Value
                Value.Map(
                    [
                        ("$policy", Value.String("If"));
                        ("condition", ifCondition);
                        ("then", ifThen);
                    ]
                )
            | 2 
            | 4 -> 
                let ifCondition = parseState.GetInput(2) :?> Value
                let ifThen = parseState.GetInput(4) :?> Value
                let ifElse = parseState.GetInput(5) :?> Value
                Value.Map(
                    [
                        ("$policy", Value.String("If"));
                        ("condition", ifCondition);
                        ("then", ifThen);
                        ("else", ifElse);
                    ]
                )
            | _ -> failwith "unexpected kind"
        valueOrHashed parseState term

    let oneOrSequence (values: Value list) : Value = 
        match values with
        | [ t; ] -> t
        | _ -> 
            Value.Map(
                [
                    ("$policy", Value.String("Sequence"));
                    ("terms", Value.List(List.rev values));
                ]
            )

    let termLet (kind: int) (parseState: IParseState) : Value = 
        let term = 
            match kind with
            | 1 -> 
                let letPattern = parseState.GetInput(2) :?> Value
                let letTerm = parseState.GetInput(4) :?> Value
                let letIn = parseState.GetInput(6) :?> Value list
                Value.Map(
                    [
                        ("$policy", Value.String("Let"));
                        ("pattern", letPattern);
                        ("term", letTerm);
                        ("in", oneOrSequence letIn);
                    ]
                )
            | 2 ->
                let letPattern = parseState.GetInput(3) :?> Value
                let letTerm = parseState.GetInput(5) :?> Value
                let letIn = parseState.GetInput(7) :?> Value list
                Value.Map(
                    [
                        ("$policy", Value.String("LetRec"));
                        ("pattern", letPattern);
                        ("term", letTerm);
                        ("in", oneOrSequence letIn);
                    ]
                )
            | _ -> failwith "unexpected kind"
        valueOrHashed parseState term

    let termSequence (parseState: IParseState)  : Value =
        let terms = parseState.GetInput(2) :?> Value list
        match terms with
        | [ t ] -> t
        | _ -> 
            let term = 
                Value.Map(
                    [
                        ("$policy", Value.String("Sequence"));
                        ("terms", Value.List(List.rev terms));
                    ]
                )
            valueOrHashed parseState term

    let termTuple (parseState: IParseState)  : Value =
        let terms = parseState.GetInput(2) :?> Value list
        match terms with
        | [ t ] -> t
        | _ -> 
            let term = Value.List(terms)
            valueOrHashed parseState term

    let termEqual (parseState: IParseState)  : Value =
        let infixLeft = parseState.GetInput(1) :?> Value
        let infixOperator = "="
        let infixRight = parseState.GetInput(3) :?> Value
        let term = 
            Value.Map(
                [
                    ("$policy", Value.String("Infix"));
                    ("operator", Value.String(infixOperator));
                    ("left", infixLeft);
                    ("right", infixRight);
                ]
            )
        valueOrHashed parseState term

    let termInfix (parseState: IParseState)  : Value =
        let infixLeft = parseState.GetInput(1) :?> Value
        let infixOperator = parseState.GetInput(2) :?> string
        let infixRight = parseState.GetInput(3) :?> Value
        let term = 
            Value.Map(
                [
                    ("$policy", Value.String("Infix"));
                    ("operator", Value.String(infixOperator));
                    ("left", infixLeft);
                    ("right", infixRight);
                ]
            )
        valueOrHashed parseState term

    let termRule (kind: int) (parseState: IParseState) : Value = 
        let term = 
            match kind with
            | 1 -> 
                let rulePattern = parseState.GetInput(1) :?> Value
                let ruleTerm = parseState.GetInput(3) :?> Value
                Value.Map(
                    [
                        ("$policy", Value.String("Rule"));
                        ("pattern", rulePattern);
                        ("term", ruleTerm);
                    ]
                )
            | 2 ->
                let rulePattern = parseState.GetInput(1) :?> Value
                let ruleGuard = parseState.GetInput(3) :?> Value
                let ruleTerm = parseState.GetInput(5) :?> Value
                Value.Map(
                    [
                        ("$policy", Value.String("Rule"));
                        ("pattern", rulePattern);
                        ("guard", ruleGuard);
                        ("term", ruleTerm);
                    ]
                )
            | _ -> failwith "unexpected kind"
        valueOrHashed parseState term

    let termMatch (kind: int) (parseState: IParseState) : Value = 
        let rulesIndex = if kind = 1 then 4 else 5
        let matchTerm = parseState.GetInput(2) :?> Value
        let matchRules = parseState.GetInput(rulesIndex) :?> Value list
        let term = 
            Value.Map(
                [
                    ("$policy", Value.String("Match"));
                    ("term", matchTerm);
                    ("rules", Value.List(List.rev matchRules));
                ]
            )
        valueOrHashed parseState term

    let termPolicy (kind: int) (parseState: IParseState) : Value = 
        let rulesIndex = if kind = 1 then 4 else 5
        let policyTerm = parseState.GetInput(2) :?> Value
        let policyRules = parseState.GetInput(rulesIndex) :?> Value list
        let term = 
            Value.Map(
                [
                    ("$policy", Value.String("Policy"));
                    ("term", policyTerm);
                    ("rules", Value.List(List.rev policyRules));
                ]
            )
        valueOrHashed parseState term

    let termReceive (kind: int) (parseState: IParseState) : Value = 
        let rulesIndex = if kind = 1 then 5 else 6
        let receiveTerm = parseState.GetInput(3) :?> Value
        let receiveRules = parseState.GetInput(rulesIndex) :?> Value list
        let term = 
            Value.Map(
                [
                    ("$policy", Value.String("Receive"));
                    ("term", receiveTerm);
                    ("rules", Value.List(List.rev receiveRules));
                ]
            )
        valueOrHashed parseState term

    let termSend (parseState: IParseState) : Value = 
        let sendMessage = parseState.GetInput(2) :?> Value
        let sendChannel = parseState.GetInput(4) :?> Value
        let term = 
            Value.Map(
                [
                    ("$policy", Value.String("Send"));
                    ("message", sendMessage);
                    ("channel", sendChannel);
                ]
            )
        valueOrHashed parseState term

    let termTryWith (parseState: IParseState) : Value = 
        let tryTerm = parseState.GetInput(2) :?> Value
        let tryRules = parseState.GetInput(4) :?> Value list
        let term = 
            Value.Map(
                [
                    ("$policy", Value.String("TryWith"));
                    ("term", tryTerm);
                    ("rules", Value.List(List.rev tryRules));
                ]
            )
        valueOrHashed parseState term

    let termTryFinally (parseState: IParseState) : Value = 
        let tryTerm = parseState.GetInput(2) :?> Value
        let tryFinally = parseState.GetInput(4) :?> Value
        let term = 
            Value.Map(
                [
                    ("$policy", Value.String("TryFinally"));
                    ("term", tryTerm);
                    ("finally", tryFinally);
                ]
            )
        valueOrHashed parseState term

    let termRef (parseState: IParseState) : Value = 
        let refTerm = parseState.GetInput(2) :?> Value
        let term = 
            Value.Map(
                [
                    ("$policy", Value.String("Ref"));
                    ("term", refTerm);
                ]
            )
        valueOrHashed parseState term

    let termDereference (parseState: IParseState) : Value = 
        let derefTerm = parseState.GetInput(2) :?> Value
        let term = 
            Value.Map(
                [
                    ("$policy", Value.String("Dereference"));
                    ("term", derefTerm);
                ]
            )
        valueOrHashed parseState term

    let termAssignment (parseState: IParseState) : Value = 
        let assignmentRef = parseState.GetInput(1) :?> Value
        let assignmentValue = parseState.GetInput(3) :?> Value
        let term = 
            Value.Map(
                [
                    ("$policy", Value.String("Assignment"));
                    ("ref", assignmentRef);
                    ("value", assignmentValue);
                ]
            )
        valueOrHashed parseState term

    let termLookup (parseState: IParseState) : Value = 
        let lookupName = parseState.GetInput(1) :?> string
        let term = 
            Value.Map(
                [
                    ("$policy", Value.String("Lookup"));
                    ("name", Value.String(lookupName));
                ]
            )
        valueOrHashed parseState term

    let termParallel (index: int) (parseState: IParseState) : Value =
        let parallelTerm = parseState.GetInput(index) :?> Value
        let term = 
            Value.Map(
                [
                    ("$policy", Value.String("Parallel"));
                    ("term", parallelTerm);
                ]
            )
        valueOrHashed parseState term

    let keyValueParallel (index: int) (parseState: IParseState) : (string * Value) =
        let (key, value) = parseState.GetInput(index) :?> (string * Value)
        let parallelValue = 
            let term = 
                Value.Map(
                    [
                        ("$policy", Value.String("Parallel"));
                        ("term", value);
                    ]
                )
            valueOrHashed parseState term
        (key, parallelValue)

    let termMap (kind: int) (parseState: IParseState)  : Value =
        let term = 
            match kind with
            | 1 -> 
                Value.Map([])
            | 2 ->
                let members = parseState.GetInput(2) :?> (string * Value) list
                Value.Map(List.rev members)
            | _ ->
                failwith "unexpected kind"
        valueOrHashed parseState term

    let termList (kind: int) (parseState: IParseState)  : Value =
        let term = 
            match kind with
            | 1 -> 
                Value.List([])
            | 2 ->
                let elements = parseState.GetInput(2) :?> Value list
                Value.List(List.rev elements)
            | _ ->
                failwith "unexpected kind"
        valueOrHashed parseState term