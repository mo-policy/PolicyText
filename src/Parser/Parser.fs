// Copyright (c) Mobile Ownership, mobileownership.org.  All Rights Reserved.  See LICENSE.txt in the project root for license information.

namespace MobileOwnership.PolicyText

open FSharp.Text.Lexing

module Parser =
    let lexText text: Grammar.token seq =
        let lexbuf = LexBuffer<char>.FromString(text)
        seq {
            while not(lexbuf.IsPastEndOfStream) do
                yield (Lexer.policy lexbuf)
        }

    let parseTextDebug text =
        let lexbuf = LexBuffer<char>.FromString(text)
        let lexer : (LexBuffer<char> -> Grammar.token) = 
            fun buf -> 
                let token = Lexer.policy buf
                printfn "%A" token
                token

        let expr = Grammar.policyText lexer lexbuf
        expr

    let parseTextHashed text = 
        let lexbuf = LexBuffer<char>.FromString(text)
        lexbuf.BufferLocalStore.Add("hashed", true)
        let expr = Grammar.policyText Lexer.policy lexbuf
        expr

    let parseText text =
        let lexbuf = LexBuffer<char>.FromString(text)
        let expr = Grammar.policyText Lexer.policy lexbuf
        expr

    let parseFile filename =
        use tr = System.IO.File.OpenText(filename)
        let lexbuf = LexBuffer<char>.FromTextReader(tr)
        let expr = Grammar.policyText Lexer.policy lexbuf
        expr
