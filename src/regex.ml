open Regex_base

let rec repeat n l =
  if n=0 then []
  else l @ repeat (n-1) l

let rec expr_repeat n e =
  if n <= 0 then
    Eps  (* Expression vide si n est inférieur ou égal à 0 *)
  else if n = 1 then
    e  (* L'expression e elle-même si n est égal à 1 *)
  else
    Concat (e, expr_repeat (n - 1) e)  (* Concaténation de e avec n-1 occurrences de e *)


let rec is_empty e =
  match e with
  | Eps -> true  (* Le mot vide est reconnu par Eps *)
  | Base _ -> false  (* Une base contient au moins un symbole *)
  | Joker -> false  (* Le Joker reconnaît le mot vide car il peut représenter n'importe quel symbole *)
  | Concat (e1, e2) -> is_empty e1 && is_empty e2  (* Si les deux sous-expressions reconnaissent le mot vide, la concaténation le fait aussi *)
  | Alt (e1, e2) ->  is_empty e1 && is_empty e2  (* il faut voir que les 2 mots ne sont pas vide *)
  | Star t -> is_empty t  
(* Une étoile reconnaît le mot vide car elle peut être répétée zéro fois *)


let rec null e =
  match e with
  | Eps -> true  (* Le mot vide est reconnu par Eps *)
  | Base _ -> false  (* Une base contient au moins un symbole, donc le mot vide n'est pas reconnu *)
  | Joker -> false  (* Le Joker  *)
  | Concat (e1, e2) -> null e1 && null e2  (* Si les deux sous-expressions reconnaissent le mot vide, la concaténation le fait aussi *)
  | Alt (e1, e2) -> null e1 || null e2  (* L'alternative reconnaît le mot vide si au moins l'une des sous-expressions le fait *)
  | Star _ -> true  (* Une étoile reconnaît le mot vide car elle peut être répétée zéro fois *)


let rec is_finite e =
  failwith "À compléter"

let product l1 l2 =
  failwith "À compléter"

let enumerate alphabet e =
  failwith "À compléter"

let rec alphabet_expr e =
  failwith "À compléter"

type answer =
  Infinite | Accept | Reject

let accept_partial e w =
  failwith "À compléter"
