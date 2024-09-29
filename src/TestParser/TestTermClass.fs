// Copyright (c) Mobile Ownership, mobileownership.org.  All Rights Reserved.  See LICENSE.txt in the project root for license information.

namespace MobileOwnership.PolicyText.TestParser

open Microsoft.VisualStudio.TestTools.UnitTesting
open MobileOwnership.PolicyText

[<TestClass>]
type TestTermClass() =
    let testSequence() : (string * string) = 
        let text = "begin 1; 2 end"
        let js = """ 
            {
                $policy: "Sequence",
                terms: [
                    1,
                    2
                ]
            }"""
        (text, js)

    let testPolicyNestedInner() : (string * string) = 
        let text = """
            policy begin
                policy 1 with
                | 1 -> 2
            end with 
            | 2 -> 3"""
        let js = """
            {
                $policy: "Policy",
                term: {
                    $policy: "Policy",
                    term: 1,
                    rules: [
                        {
                            $policy: "Rule",
                            pattern: 1,
                            term: 2
                        }
                    ]
                },
                rules: [
                    {
                        $policy: "Rule",
                        pattern: 2,
                        term: 3
                    }
                ]
            }"""
        (text, js)

    let testPolicy1() : (string * string) = 
        let text = """
            policy 1 with
            | 1 -> 2"""
        let js = """
            {
                $policy: "Policy",
                term: 1,
                rules: [
                    {
                        $policy: "Rule",
                        pattern: 1,
                        term: 2
                    }
                ]
            }"""
        (text, js)

    let testInfixPlus() : (string * string) = 
        let text = "1+2"
        let js = """
            {
                $policy: "Infix",
                operator: "+",
                left: 1,
                right: 2
            }"""
        (text, js)

    let testInfixPlusMult() : (string * string) = 
        let text = "1+2*3"
        let js = "1+(2*3)"
        (text, js)

    let testInfixDivDiv() : (string * string) = 
        let text = "1/2/3"
        let js = "(1/2)/3"
        (text, js)

    [<TestMethod>]
    member _.TestParserValues() =
        let data = [
            ("{}", Value.Map([]));
            ("{ \"x\": 1 }", Value.Map([ ("x", Value.Integer(1)) ]));
            ("{ \"x\": 1, y: { z: []} }", Value.Map([ ("x", Value.Integer(1)); ("y", Value.Map([("z", Value.List([]))])) ]));
            ("[]", Value.List([]));
            ("[null]", Value.List([ Value.Null ]));
            ("[1,2, true ]", Value.List([ Value.Integer(1); Value.Integer(2); Value.Boolean(true) ]));
            ("\"Hello\"", Value.String("Hello"));
            ("-1908.045", Value.Double(-1908.045));
            ("2039", Value.Integer(2039));
            ("true", Value.Boolean(true));
            ("false", Value.Boolean(false));
            ("null", Value.Null);
        ]
        for (text, expected) in data do
            let actual = Parser.parseText text
            Assert.IsTrue((actual = expected))

    [<TestMethod>]
    member _.TestParserTerms() =
        let data = [
            testSequence;
            testPolicyNestedInner;
            testPolicy1;
        ]
        for f in data do
            let (text, js) = f()
            let actual = Parser.parseText text
            let expected = Parser.parseText js
            let isEqual = (actual = expected)
            if not(isEqual) then
                printfn "%A" actual
                printfn "%A" expected
            Assert.IsTrue(isEqual)

    [<TestMethod>]
    member _.TestParserInfix() =
        let data = [ 
            testInfixPlus;
            testInfixPlusMult;
            testInfixDivDiv;
        ]
        for f in data do
            let (text, js) = f()
            let actual = Parser.parseText text
            let expected = Parser.parseText js
            let isEqual = (actual = expected)
            if not(isEqual) then
                printfn "%A" actual
                printfn "%A" expected
            Assert.IsTrue(isEqual)

