module MobileOwnership.PolicyText.Lexer

open System
open System.Text
open FSharp.Text.Lexing
open MobileOwnership.PolicyText.Grammar/// Rule policy
val policy: cl: obj -> lexbuf: LexBuffer<char> -> token
/// Rule lexString
val lexString: cl: obj -> sb: obj -> lexbuf: LexBuffer<char> -> token
/// Rule lexIdent
val lexIdent: cl: obj -> sb: obj -> lexbuf: LexBuffer<char> -> token
/// Rule lexComment
val lexComment: cl: obj -> lexbuf: LexBuffer<char> -> token
