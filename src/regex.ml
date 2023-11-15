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
  let rec is_finite_helper e visited =
    if List.mem e visited then
      false  (* Boucle détectée, le langage est infini *)
    else
      match e with
      | Eps -> true  (* Le mot vide est reconnu et est fini *)
      | Base _ -> true  (* Une base contenant un caractère unique est finie *)
      | Joker -> true  (* Le Joker reconnaît le mot vide et est fini *)
      | Concat (e1, e2) -> is_finite_helper e1 (e :: visited) && is_finite_helper e2 (e :: visited)
      | Alt (e1, e2) -> is_finite_helper e1 (e :: visited) && is_finite_helper e2 (e :: visited)
      | Star m -> is_empty m  (* Une étoile reconnaît un langage potentiellement infini mais peut être vide  *)
  in
  is_finite_helper e []

let product l1 l2 =
  let concat_lists l1 l2 =
    List.concat_map (fun x -> List.map (fun y -> x @ y) l2) l1
  in
  concat_lists l1 l2


(*
   Cette fonction vérifie si un langage est fini en utilisant une
   approche de détection de boucle. Elle prend en paramètre une liste
   de mots (lang) et vérifie si chaque mot est fini.
*)
let is_finite_b lang =
    let visited = ref [] in
    let rec is_finite_helper l =
      if List.mem l !visited then
        false  (* Boucle détectée, le langage est infini *)
      else begin
        visited := l :: !visited;
        match l with
        | [] -> true  (* Le mot vide est reconnu et est fini *)
        | x :: xs -> is_finite_helper xs
      end
    in
    List.for_all (fun word -> is_finite_helper word) lang
  
let enumerate alphabet e =
  let is_finite_lang = ref true in (* Indicateur pour vérifier si le langage est fini , cette donnée peut varier *)
  let rec enumerate_helper e =
    match e with
    | Eps -> [[]]  (* Le mot vide est reconnu *)
    | Base c -> [[c]]  (* Un caractère unique est reconnu en tant que mot *)
    | Joker -> List.map (fun c -> [c]) alphabet  (* Le Joker reconnaît n'importe quel caractère *)
    | Concat (e1, e2) ->
      let l1 = enumerate_helper e1 in
      let l2 = enumerate_helper e2 in
      let product_result = product l1 l2 in 
      if not (is_finite_lang.contents) then (* changement de contenu potentiel*)
        is_finite_lang := is_finite_b product_result;
      product_result
    | Alt (e1, e2) ->
      let l1 = enumerate_helper e1 in
      let l2 = enumerate_helper e2 in
      let union_result = List.append l1 l2 in
      if not (is_finite_lang.contents) then
        is_finite_lang := is_finite_b union_result;
      union_result
    | Star _ ->
      is_finite_lang.contents <- false;  (* Une étoile reconnaît un langage potentiellement infini *)
      [[]]  (* Le mot vide est reconnu, mais on ne génère pas d'autres mots *)
  in
  let result = enumerate_helper e in
  if !is_finite_lang then
    Some result
  else
    None


let rec alphabet_expr e =
  let rec alphabet_expr_helper expr alphabet =
    match expr with
    | Eps -> alphabet  (* Pas de lettre dans le mot vide *)
    | Base c -> if List.mem c alphabet then alphabet else c :: alphabet
    | Joker -> alphabet  (* Le Joker ne contribue pas aux lettres *)
    | Concat (e1, e2) ->
      let alphabet1 = alphabet_expr_helper e1 alphabet in
      alphabet_expr_helper e2 alphabet1
    | Alt (e1, e2) ->
      let alphabet1 = alphabet_expr_helper e1 alphabet in
      alphabet_expr_helper e2 alphabet1
    | Star e -> alphabet_expr_helper e alphabet
  in
  List.sort_uniq compare (alphabet_expr_helper e [])


type answer =
  Infinite | Accept | Reject

let accept_partial e w =
  let al = alphabet_expr e in
  let reconnu = enumerate (List.sort_uniq compare (al@w)) e in
  match reconnu with
  |Some e ->
    if List.exists (fun v -> v = w) e then 
      Accept
    else Reject
  |None -> Infinite
