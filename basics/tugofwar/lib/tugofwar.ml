(* tokens *)
type token = A | B | X

(* val toklist_of_string : string -> token list *)
(* toklist_of_string s transforms the string s into a list of tokens *)
(* Hint: use the function explode in bin/main.ml to convert a string to a char list *)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let toklist_of_string s = 
  let rec tokenization = function
    [] -> []
    | 'A'::l -> A::(tokenization l)
    | 'B'::l -> B::(tokenization l)
    | '='::l -> X::(tokenization l)
    | _ -> failwith "Invalid string" 
  in tokenization (explode s)
;; 

(* val valid : token list -> bool *)
(* valid l is true when l is a list of tokens in the language A* X* B* *)

(* Should work, but technycally there's no check if they are both tokens in the second case
   Since there's a similar check in the third case, by inference recognizes that you have to pass it a list of tokens as input
   So there is no need for the check in the second case *)
let countOccurrences a l = List.length (List.filter (fun x -> x = a) l);;
let rec valid l = match l with
  [] -> true
  | a::b::l1 when (a != b) -> if countOccurrences a l1 == 0 then valid l1 else false
  | a::l1 when (a = A || a = X || a = B) -> valid l1
  | _ -> false
;;

(* val win : token list -> token *)
(* win l determines the winner of a tug of war game. X means tie *)

let win l = match List.length(List.filter (fun x -> x = A) l) - List.length(List.filter (fun x -> x = B) l) with
  0 -> X
  | x when x > 0 -> A
  | _ -> B
;;

(* val string_of_winner : token -> string *)
let string_of_winner w = match w with
  A -> "A"
  | B -> "B"
  | X -> "X"
;;
