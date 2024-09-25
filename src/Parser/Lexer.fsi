module internal MobileOwnership.PolicyText.Lexer

open System
open FSharp.Text.Lexing
open MobileOwnership.PolicyText.Grammar/// Rule policy
val policy: lexbuf: LexBuffer<char> -> token
