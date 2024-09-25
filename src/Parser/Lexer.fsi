module MobileOwnership.PolicyText.Lexer

open System
open System.Text
open FSharp.Text.Lexing
open MobileOwnership.PolicyText.Grammar/// Rule policy
val policy: lexbuf: LexBuffer<char> -> token
/// Rule lexString
val lexString: sb: obj -> lexbuf: LexBuffer<char> -> token
/// Rule lexIdent
val lexIdent: sb: obj -> lexbuf: LexBuffer<char> -> token
