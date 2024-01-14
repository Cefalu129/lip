{
  open Token
}

let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
let id = letter chr*
let num = ['0'-'9']|['1'-'9']['0'-'9']*

let quote = ['"']
let capital = ['A'-'Z']
let vowelsLow = ['a' 'e' 'i' 'o' 'u' 'y']
let vowelsUp = ['A' 'E' 'I' 'O' 'U' 'Y']
let vowels = vowelsLow | vowelsUp
let consonants = ['b'-'d' 'f'-'h' 'j'-'n' 'p'-'t' 'v'-'z' 'B'-'D' 'F'-'H' 'J'-'N' 'P'-'T' 'V'-'Z']
let float = num "."? num
let neg = "-"? num

let startH = '0'['x' 'X']
let hex = ['0'-'9' 'a'-'f' 'A'-'F']

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "=" { ASSIGN }
  | "+" { PLUS }
  | ";" { SEQ }
  | quote capital+ chr* quote { ATOK (Lexing.lexeme lexbuf) }
  | quote vowelsLow+ quote { BTOK (Lexing.lexeme lexbuf) }
  | quote consonants* vowels? consonants* quote { CTOK (Lexing.lexeme lexbuf) }
  | quote (num | float | neg) quote { DTOK (Lexing.lexeme lexbuf) }
  | quote startH hex+ quote { ETOK (Lexing.lexeme lexbuf) }
  | id { ID (Lexing.lexeme lexbuf) }
  | num { CONST (Lexing.lexeme lexbuf) }    
  | eof { EOF }