// Copyright (c) Mobile Ownership, mobileownership.org.  All Rights Reserved.  See LICENSE.txt in the project root for license information.

namespace MobileOwnership.PolicyText

open FSharp.Text.Lexing

module Parser =
    let parseText text =
        let lexbuf = LexBuffer<char>.FromString(text)
        let expr = Grammar.policy Lexer.policy lexbuf
        expr

    let parseFile filename =
        use tr = System.IO.File.OpenText(filename)
        let lexbuf = LexBuffer<char>.FromTextReader(tr)
        let expr = Grammar.policy Lexer.policy lexbuf
        expr
