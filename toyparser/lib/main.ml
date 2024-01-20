open Ast

(* parse : string -> ast *)

let parse (s : string) : ast =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read_token lexbuf in
  ast

type result = int

let string_of_result n = match n with
    Some n -> string_of_int n
  | None -> "Division by zero"
    
(* eval : ast -> result *)
    
let rec eval = function
    Const(n) -> Some n
  | Add(e1,e2) -> Some (Option.get (eval e1) + Option.get (eval e2))
  | Sub(e1,e2) -> Some (Option.get (eval e1) - Option.get (eval e2))
  | Mul(e1,e2) -> Some (Option.get (eval e1) * Option.get (eval e2))
  | Div(_,e2) when Option.get (eval e2) = 0 -> None
  | Div(e1,e2) -> Some (Option.get (eval e1) / Option.get (eval e2))

                    
