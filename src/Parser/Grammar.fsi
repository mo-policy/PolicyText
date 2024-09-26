// Signature file for parser generated by fsyacc
module MobileOwnership.PolicyText.Grammar
type token = 
  | EOF
  | NULL
  | COMMA
  | COLON
  | LBRACKET
  | RBRACKET
  | LBRACE
  | RBRACE
  | BOOLEAN of (bool)
  | DOUBLE of (double)
  | INTEGER of (int64)
  | STRING of (string)
  | IDENT of (string)
type tokenId = 
    | TOKEN_EOF
    | TOKEN_NULL
    | TOKEN_COMMA
    | TOKEN_COLON
    | TOKEN_LBRACKET
    | TOKEN_RBRACKET
    | TOKEN_LBRACE
    | TOKEN_RBRACE
    | TOKEN_BOOLEAN
    | TOKEN_DOUBLE
    | TOKEN_INTEGER
    | TOKEN_STRING
    | TOKEN_IDENT
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startpolicy
    | NONTERM_policy
    | NONTERM_term
    | NONTERM_object
    | NONTERM_members
    | NONTERM_member
    | NONTERM_array
    | NONTERM_elements
/// This function maps tokens to integer indexes
val tagOfToken: token -> int

/// This function maps integer indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val policy : (FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> FSharp.Text.Lexing.LexBuffer<'cty> -> (Value) 
