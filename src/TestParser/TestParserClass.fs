// Copyright (c) Mobile Ownership, mobileownership.org.  All Rights Reserved.  See LICENSE.txt in the project root for license information.

namespace MobileOwnership.PolicyText.TestParser

open Microsoft.VisualStudio.TestTools.UnitTesting
open MobileOwnership.PolicyText
open MobileOwnership.PolicyText.Grammar

[<TestClass>]
type TestParserClass() =

    let data = [
        ("{}", Value.Object([]));
        ("{ \"x\": 1 }", Value.Object([ ("x", Value.Integer(1)) ]));
        ("{ \"x\": 1, y: { z: []} }", Value.Object([ ("x", Value.Integer(1)); ("y", Value.Object([("z", Value.Array([]))])) ]));
        ("[]", Value.Array([]));
        ("[null]", Value.Array([ Value.Null ]));
        ("[1,2, true ]", Value.Array([ Value.Integer(1); Value.Integer(2); Value.Boolean(true) ]));
        ("\"Hello\"", Value.String("Hello"));
        ("-1908.045", Value.Double(-1908.045));
        ("2039", Value.Integer(2039));
        ("true", Value.Boolean(true));
        ("false", Value.Boolean(false));
        ("null", Value.Null);
    ]

    [<TestMethod>]
    member _.TestParser() =
        for (text, expected) in data do
            let actual = Parser.parseText text
            Assert.IsTrue((actual = expected));
