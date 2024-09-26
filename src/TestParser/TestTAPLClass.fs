// Copyright (c) Mobile Ownership, mobileownership.org.  All Rights Reserved.  See LICENSE.txt in the project root for license information.

namespace MobileOwnership.PolicyText.TestParser

open Microsoft.VisualStudio.TestTools.UnitTesting
open MobileOwnership.PolicyText

[<TestClass>]
type TestTAPLClass() =
    // (#1) These tests are based on examples found in the book
    // by Pierce, Benjamin C. "Types and Programming Languages" MIT Press
    // ISBN 978-0-262-30382-8

    // Church Booleans
    // from 5.2 Programming in the Lambda - Calculus(#1)
    let truTAPL() : (Value * Value) = 
        let text = "fun t -> fun f -> t"
        let js = """ 
            {
                $policy: "Function",
                pattern: { $policy: "Lookup", name: "t" },
                term: {
                    $policy: "Function",
                    pattern: { $policy: "Lookup", name: "f" },
                    term: { $policy: "Lookup", name: "t" }
                }
            }"""
        let truActual = Parser.parseText text
        let truExpected = Parser.parseText js
        (truActual, truExpected)

    let flsTAPL() : (Value * Value) = 
        let text = "fun t -> fun f -> f"
        let js = """ 
            {
                $policy: "Function",
                pattern: { $policy: "Lookup", name: "t" },
                term: {
                    $policy: "Function",
                    pattern: { $policy: "Lookup", name: "f" },
                    term: { $policy: "Lookup", name: "f" }
                }
            }"""
        let truActual = Parser.parseText text
        let truExpected = Parser.parseText js
        (truActual, truExpected)

    let testTAPL() : (Value * Value) = 
        let text = "fun l -> fun m -> fun n -> l m n"
        let js = """ 
            {
                $policy: "Function",
                pattern: { $policy: "Lookup", name: "l" },
                term: {
                    $policy: "Function",
                    pattern: { $policy: "Lookup", name: "m" },
                    term: {
                        $policy: "Function",
                        pattern: { $policy: "Lookup", name: "n" },
                        term: {
                            $policy: "Application",
                            function: {
                                $policy: "Application",
                                function: { $policy: "Lookup", name: "l" },
                                arg: { $policy: "Lookup", name: "m" }
                            },
                            arg: { $policy: "Lookup", name: "n" }
                        }
                    }
                }
            }"""
        let truActual = Parser.parseText text
        let truExpected = Parser.parseText js
        (truActual, truExpected)

    let andTAPL() : (Value * Value) = 
        let text = "fun b -> fun c -> b c fls"
        let js = """ 
            {
                $policy: "Function",
                pattern: { $policy: "Lookup", name: "b" },
                term: {
                    $policy: "Function",
                    pattern: { $policy: "Lookup", name: "c" },
                    term: {
                        $policy: "Application",
                        function: {
                            $policy: "Application",
                            function: { $policy: "Lookup", name: "b" },
                            arg: { $policy: "Lookup", name: "c" }
                        },
                        arg: fls
                    }
                }
            }"""
        let truActual = Parser.parseText text
        let truExpected = Parser.parseText js
        (truActual, truExpected)

    let orTAPL() : (Value * Value) = 
        let text = "fun b -> fun c -> b tru c"
        let js = """ 
            {
                $policy: "Function",
                pattern: { $policy: "Lookup", name: "b" },
                term: {
                    $policy: "Function",
                    pattern: { $policy: "Lookup", name: "c" },
                    term: {
                        $policy: "Application",
                        function: {
                            $policy: "Application",
                            function: { $policy: "Lookup", name: "b" },
                            arg: { $policy: "Lookup", name: "tru" }
                        },
                        arg: { $policy: "Lookup", name: "c" }
                    }
                }
            }"""
        let truActual = Parser.parseText text
        let truExpected = Parser.parseText js
        (truActual, truExpected)

    let notTAPL() : (Value * Value) = 
        let text = "fun b -> b fls tru"
        let js = """ 
            {
                $policy: "Function",
                pattern: { $policy: "Lookup", name: "b" },
                term: {
                    $policy: "Application",
                    function: {
                        $policy: "Application",
                        function: { $policy: "Lookup", name: "b" },
                        arg: { $policy: "Lookup", name: "fls" }
                    },
                    arg: { $policy: "Lookup", name: "tru" }
                }
            }"""
        let truActual = Parser.parseText text
        let truExpected = Parser.parseText js
        (truActual, truExpected)

    let pairTAPL() : (Value * Value) =
        let text = "fun f -> fun s -> fun b -> b f s"
        let js = """ 
            {
                $policy: "Function",
                pattern: { $policy: "Lookup", name: "f" },
                term: {
                    $policy: "Function",
                    pattern: { $policy: "Lookup", name: "s" },
                    term: {
                        $policy: "Function",
                        pattern: { $policy: "Lookup", name: "b" },
                        term: {
                            $policy: "Application",
                            function: {
                                $policy: "Application",
                                function: { $policy: "Lookup", name: "b" },
                                arg: { $policy: "Lookup", name: "f" }
                            },
                            arg: { $policy: "Lookup", name: "s" }
                        }
                    }
                }
            }"""
        let truActual = Parser.parseText text
        let truExpected = Parser.parseText js
        (truActual, truExpected)

    [<TestMethod>]
    member _.TestParserTAPL() = 
        let data = [
            pairTAPL;
            notTAPL;
            orTAPL;
            andTAPL;
            testTAPL;
            flsTAPL;
            truTAPL;
        ]
        for f in data do
            let (actual, expected) = f()
            let isEqual = (actual = expected)
            if not(isEqual) then
                printfn "%A" actual
                printfn "%A" expected
            Assert.IsTrue(isEqual)
