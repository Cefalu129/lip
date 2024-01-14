let rec lang1 = function
  [] -> false
  | a::[] when (a = '0' || a = '1') -> true
  | '0'::l1 -> lang1 l1
  | '1'::l2 -> lang1 l2
  | _ -> false
;;

let countOccurrences a l = List.length (List.filter (fun x -> x = a) l);;
let rec lang2 = function
  [] -> true
  | '0'::l1 -> if (countOccurrences '0' l1) > 0 then false else lang2 l1
  | '1'::l2 -> if (countOccurrences '0' l2) > 0 then false else lang2 l2
  | _ -> false
;;

let rec checkBody = function
  [] -> false
  | '0'::[] -> true
  | a::l1 when (a = '0' || a = '1') -> checkBody l1
  | _ -> false  
;;
let lang3 = function
  [] -> false
  | '0'::l1 -> checkBody l1
  | _ -> false

let lang4 l = if List.length(List.filter(fun x -> x = '0' || x = '1') l) == List.length l then countOccurrences '1' l == 2 else false;;

let rec lang5 = function
  [] -> false
  | a::b::[] when ((a=b) && (a='0' || a='1')) -> true
  | a::b::c when ((a=b) && (a='0' || a='1')) -> lang5 c
  | _ -> false
    
let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers
  
