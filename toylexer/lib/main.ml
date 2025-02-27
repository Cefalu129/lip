open Token
    
(* tokenize : Lexing.lexbuf -> LexingLib.Token.token list *)

let rec tokenize lexbuf =
  match Lexer.read_token lexbuf with
    EOF -> [EOF]
  | t -> t::(tokenize lexbuf)

(* lexer : string -> LexingLib.Token.token list *)

let lexer (s : string) =
  let lexbuf = Lexing.from_string s in
  tokenize lexbuf

(* string_of_tokenlist : token list -> string *)
    
let string_of_tokenlist tl = 
  List.fold_left (fun s t -> s ^ (string_of_token t ^ (if t=EOF then "" else " "))) "" tl

(* string_of_frequencies : (token * int) list -> string *)
    
let string_of_frequencies fl =
  List.fold_left (fun s (t,n) -> s ^ ((string_of_token t) ^ " -> " ^ string_of_int n ^ "\n")) "" fl

(* frequency : int -> 'a list -> ('a * int) list *)
let countElement e l = List.length(List.filter (fun x -> x = e) l);;

let rec splitAtElement = function
    (0, _) -> []
    | (_, []) -> []
    | (x, a::l) -> a::(splitAtElement (x-1, l))
;;

let frequency i l = 
  let rec makeList = function
    | [] -> []
    | a::l1 -> (a, countElement a l)::(makeList (List.filter (fun x -> x <> a) l1))
  in splitAtElement (i, List.sort (fun (_,a) (_,b) -> compare b a) (makeList l))
;;
