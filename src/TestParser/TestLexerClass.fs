// Copyright (c) Mobile Ownership, mobileownership.org.  All Rights Reserved.  See LICENSE.txt in the project root for license information.

namespace MobileOwnership.PolicyText.TestParser

open Microsoft.VisualStudio.TestTools.UnitTesting
open MobileOwnership.PolicyText
open MobileOwnership.PolicyText.Grammar

[<TestClass>]
type TestLexerClass() =
    let data = [
        ("", [ EOF ]);
        ("        \r\n", [ EOF ]);
        ("  // hello\r\n", [ EOF ]);
        ("(* \r\n ``hello\r\n *)", [ EOF ]);
        ("(* \" *) \" *)", [ EOF ]);
        ("(* let x = \" *) \" *)", [ EOF ]);
        ("(* lety x = \" *) \" *)", [ EOF ]);
        ("(* (* nested *) *) x", [ IDENT("x"); EOF ]);
        ("(* (* nested // *) *) x", [ IDENT("x"); EOF ]);
        ("X", [ IDENT("X"); EOF ]);
        ("_", [ IDENT("_"); EOF ]);
        ("_12xj", [ IDENT("_12xj"); EOF ]);
        ("____", [ IDENT("____"); EOF ]);
        ("``_``", [ IDENT("_"); EOF ]);
        ("``long identifier'&^hello``", [ IDENT("long identifier'&^hello"); EOF ]);
        ("0", [ INTEGER(0); EOF ]);
        ("129", [ INTEGER(129); EOF ]);
        ("-1", [ INTEGER(-1); EOF ]);
        ("-9098", [ INTEGER(-9098); EOF ]);
        ("0.0", [ DOUBLE(0.0); EOF ]);
        ("14561E11", [ DOUBLE(14561E11); EOF ]);
        ("-1.9e-3", [ DOUBLE(-1.9e-3); EOF ]);
        ("0.0134", [ DOUBLE(0.0134); EOF ]);
        ("\"\"", [ STRING(""); EOF ]);
        ("\"\\t\"", [ STRING("\t"); EOF ]);
        ("\"\\u0035\"", [ STRING("\u0035"); EOF ]);
        ("\"\\/\"", [ STRING("/"); EOF ]);
        ("\"/\"", [ STRING("/"); EOF ]);
        ("\"12345-hello\"", [ STRING("12345-hello"); EOF ]);
        ("\"λx\"", [ STRING("λx"); EOF ]);
        ("\"   ~.()!\"", [ STRING("   ~.()!"); EOF ]);
        ("\" \\r\\n\\t\\b  abc\"", [ STRING(" \r\n\t\b  abc"); EOF ]);
        ("\"0\"", [ STRING("0"); EOF ]);
        ("{}", [ LBRACE; RBRACE; EOF ]);
        ("{\"x\": 1}", [ LBRACE; STRING("x"); COLON; INTEGER(1); RBRACE; EOF ]);
        ("{\"x\": 1, y:\"y\"}", [ LBRACE; STRING("x"); COLON; INTEGER(1); COMMA; IDENT("y"); COLON; STRING("y"); RBRACE; EOF ]);
        ("[  \n\r  ]", [ LBRACKET; RBRACKET; EOF ]);
        ("[ 1, true, false, \r\n null  ]", [ LBRACKET; INTEGER(1); COMMA; BOOLEAN(true); COMMA; BOOLEAN(false); COMMA; NULL; RBRACKET; EOF ]);
    ]

    [<TestMethod>]
    member _.TestLexer() =
        for (text, expected) in data do
            let actual = List.ofSeq (Parser.lexText text)
            Assert.IsTrue((actual = expected));
