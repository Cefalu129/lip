{
  open Parser
}

let white = [' ' '\t']+
let num = ['0'-'9']|['1'-'9']['0'-'9']*

let hexPrefix = "0x"|"0X"
let hex = hexPrefix ['0'-'9' 'a'-'f' 'A'-'F']*

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { MUL }
  | "/" { DIV }
  | hex | num { CONST (Lexing.lexeme lexbuf) }
  | eof { EOF }
