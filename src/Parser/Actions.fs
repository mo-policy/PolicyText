// Copyright (c) Mobile Ownership, mobileownership.org.  All Rights Reserved.  See LICENSE.txt in the project root for license information.

namespace MobileOwnership.PolicyText

open FSharp.Text.Parsing

[<RequireQualifiedAccess>]
type Value =
    | Map           of (string * Value) list
    | List          of Value list
    | String        of string
    | Integer       of int64
    | Double        of double
    | Boolean       of bool
    | Null

module Actions = 
    let termSequence (parseState: IParseState)  : Value =
        let terms = parseState.GetInput(1) :?> Value list
        match terms with
        | [ t ] -> t
        | _ -> 
            Value.Map(
                [
                    ("$policy", Value.String("Sequence"));
                    ("terms", Value.List(List.rev terms));
                ]
            )

    let termApplication (parseState: IParseState) : Value = 
        let appFunction = parseState.GetInput(1) :?> Value
        let appArg = parseState.GetInput(2) :?> Value
        Value.Map(
            [
                ("$policy", Value.String("Application"));
                ("function", appFunction);
                ("arg", appArg);
            ]
        )

    let termFunction (parseState: IParseState) : Value = 
        let funPattern = parseState.GetInput(2) :?> Value
        let funTerm = parseState.GetInput(4) :?> Value
        Value.Map(
            [
                ("$policy", Value.String("Function"));
                ("pattern", funPattern);
                ("term", funTerm);
            ]
        )

    let termIf (kind: int) (parseState: IParseState) : Value = 
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

    let termLet (kind: int) (parseState: IParseState) : Value = 
        match kind with
        | 1 -> 
            let letPattern = parseState.GetInput(2) :?> Value
            let letTerm = parseState.GetInput(4) :?> Value
            let letIn = parseState.GetInput(6) :?> Value
            Value.Map(
                [
                    ("$policy", Value.String("Let"));
                    ("pattern", letPattern);
                    ("term", letTerm);
                    ("in", letIn);
                ]
            )
        | 2 ->
            let letPattern = parseState.GetInput(3) :?> Value
            let letTerm = parseState.GetInput(5) :?> Value
            let letIn = parseState.GetInput(7) :?> Value
            Value.Map(
                [
                    ("$policy", Value.String("LetRec"));
                    ("pattern", letPattern);
                    ("term", letTerm);
                    ("in", letIn);
                ]
            )
        | _ -> failwith "unexpected kind"

    let termRule (kind: int) (parseState: IParseState) : Value = 
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

    let termMatch (kind: int) (parseState: IParseState) : Value = 
        let rulesIndex = if kind = 1 then 4 else 5
        let matchTerm = parseState.GetInput(2) :?> Value
        let matchRules = parseState.GetInput(rulesIndex) :?> Value list
        Value.Map(
            [
                ("$policy", Value.String("Match"));
                ("term", matchTerm);
                ("rules", Value.List(List.rev matchRules));
            ]
        )

    let termPolicy (kind: int) (parseState: IParseState) : Value = 
        let rulesIndex = if kind = 1 then 4 else 5
        let policyTerm = parseState.GetInput(2) :?> Value
        let policyRules = parseState.GetInput(rulesIndex) :?> Value list
        Value.Map(
            [
                ("$policy", Value.String("Policy"));
                ("term", policyTerm);
                ("rules", Value.List(List.rev policyRules));
            ]
        )

    let termReceive (kind: int) (parseState: IParseState) : Value = 
        let rulesIndex = if kind = 1 then 5 else 6
        let receiveTerm = parseState.GetInput(3) :?> Value
        let receiveRules = parseState.GetInput(rulesIndex) :?> Value list
        Value.Map(
            [
                ("$policy", Value.String("Receive"));
                ("term", receiveTerm);
                ("rules", Value.List(List.rev receiveRules));
            ]
        )

    let termSend (parseState: IParseState) : Value = 
        let sendMessage = parseState.GetInput(2) :?> Value
        let sendChannel = parseState.GetInput(4) :?> Value
        Value.Map(
            [
                ("$policy", Value.String("Send"));
                ("message", sendMessage);
                ("channel", sendChannel);
            ]
        )

    let termTryWith (parseState: IParseState) : Value = 
        let tryTerm = parseState.GetInput(2) :?> Value
        let tryRules = parseState.GetInput(4) :?> Value list
        Value.Map(
            [
                ("$policy", Value.String("TryWith"));
                ("term", tryTerm);
                ("rules", Value.List(List.rev tryRules));
            ]
        )

    let termTryFinally (parseState: IParseState) : Value = 
        let tryTerm = parseState.GetInput(2) :?> Value
        let tryFinally = parseState.GetInput(4) :?> Value
        Value.Map(
            [
                ("$policy", Value.String("TryFinally"));
                ("term", tryTerm);
                ("finally", tryFinally);
            ]
        )

    let termRef (parseState: IParseState) : Value = 
        let refTerm = parseState.GetInput(2) :?> Value
        Value.Map(
            [
                ("$policy", Value.String("Ref"));
                ("term", refTerm);
            ]
        )

    let termDereference (parseState: IParseState) : Value = 
        let derefTerm = parseState.GetInput(2) :?> Value
        Value.Map(
            [
                ("$policy", Value.String("Dereference"));
                ("term", derefTerm);
            ]
        )

    let termAssignment (parseState: IParseState) : Value = 
        let assignmentRef = parseState.GetInput(1) :?> Value
        let assignmentValue = parseState.GetInput(3) :?> Value
        Value.Map(
            [
                ("$policy", Value.String("Assignment"));
                ("ref", assignmentRef);
                ("value", assignmentValue);
            ]
        )

    let termLookup (parseState: IParseState) : Value = 
        let lookupName = parseState.GetInput(1) :?> string
        Value.Map(
            [
                ("$policy", Value.String("Lookup"));
                ("name", Value.String(lookupName));
            ]
        )

    let termMap (kind: int) (parseState: IParseState)  : Value =
        match kind with
        | 1 -> 
            Value.Map([])
        | 2 ->
            let members = parseState.GetInput(2) :?> (string * Value) list
            Value.Map(List.rev members)
        | _ ->
            failwith "unexpected kind"

    let termList (kind: int) (parseState: IParseState)  : Value =
        match kind with
        | 1 -> 
            Value.List([])
        | 2 ->
            let elements = parseState.GetInput(2) :?> Value list
            Value.List(List.rev elements)
        | _ ->
            failwith "unexpected kind"