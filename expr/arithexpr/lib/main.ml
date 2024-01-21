open Ast

let rec string_of_expr = function
    True -> "True"
  | False -> "False"
  | Not e -> "Not(" ^ (string_of_expr e) ^ ")"
  | And(e1,e2) -> "And(" ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Or(e1,e2) -> "Or(" ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | If(e0,e1,e2) -> "If(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Zero -> "0"
  | Succ e -> "Succ(" ^ (string_of_expr e) ^ ")"
  | Pred e -> "Pred(" ^ (string_of_expr e) ^ ")"
  | IsZero e -> "IsZero(" ^ (string_of_expr e) ^ ")"
;;

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

exception NoRuleApplies

let rec is_nv = function
    Zero -> true
  | Succ e -> is_nv e
  | _ -> false
  
let rec trace1 = function
    If(True,e1,_) -> e1
  | If(False,_,e2) -> e2
  | If(e0,e1,e2) -> let e0' = trace1 e0 in If(e0',e1,e2)
  | Not(True) -> False
  | Not(False) -> True
  | Not(e) -> let e' = trace1 e in Not(e')
  | And(True,e2) -> e2
  | And(False,_) -> False
  | And(e1,e2) -> let e1' = trace1 e1 in And(e1',e2)
  | Or(True,_) -> True
  | Or(False,e2) -> e2
  | Or(e1,e2) -> let e1' = trace1 e1 in Or(e1',e2)
  | Succ(e) -> let e' = trace1 e in Succ(e')
  | Pred(Succ nv) when is_nv nv -> nv
  | Pred(e) -> let e' = trace1 e in Pred(e')
  | IsZero(Zero) -> True
  | IsZero(Succ nv) when is_nv nv -> False
  | IsZero(e) -> let e' = trace1 e in IsZero(e')
  | _ -> raise NoRuleApplies
;;

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]
;;

type expravl = Bool of bool | Nat of int ;;

exception InvalidArg of string

let string_of_val = function
    Bool b -> string_of_bool b
  | Nat n -> string_of_int n
;;

let get_nat = function
    Nat x -> x
  | _ -> raise (InvalidArg "Not a natural number")
;;

let get_bool = function
    Bool b -> b
  | _ -> raise (InvalidArg "Not a boolean")
;;

let rec eval = function
    True -> Bool true
  | False -> Bool false
  | Not e -> Bool (not (get_bool (eval e)))
  | And(e1,e2) -> Bool ((get_bool (eval e1)) && (get_bool (eval e2)))
  | Or(e1,e2) -> Bool ((get_bool (eval e1)) || (get_bool (eval e2)))
  | If(e0,e1,e2) -> if (get_bool (eval e0)) then (eval e1) else (eval e2)
  | Zero -> Nat 0
  | Succ e -> Nat ((get_nat (eval e)) + 1)
  | Pred e when get_nat (eval e) = 0 -> raise (InvalidArg "Negative")
  | Pred e -> Nat ((get_nat (eval e)) - 1)
  | IsZero e -> Bool ((get_nat (eval e)) = 0)
;;
