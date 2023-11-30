type base = A | C | G | T | WC (* wildcard *)

type dna = base list



(*---------------------------------------------------------------------------*)
(*                               ECHAUFFEMENT                                *)
(*---------------------------------------------------------------------------*)


let string_of_base (b : base) : string =
  match b with
    A -> "A"
  | C -> "C"
  | G -> "G"
  | T -> "T"
  | WC -> "."


(* explode a string into a char list *)
let explode (str : string) : char list =
  let rec aux index acc =
    if index < 0 then acc
    else aux (index - 1) (str.[index] :: acc)
  in
  aux (String.length str - 1) []


(* conversions *)
let base_of_char (c : char) : base = match c with
| 'A' ->  A
| 'C' ->  C
| 'G' ->  G
| 'T' ->  T
| _ ->  WC


let dna_of_string (s : string) : base list =
  List.map base_of_char (explode s)

let string_of_dna (seq : dna) : string =
  String.concat "" (List.map string_of_base seq)

(*---------------------------------------------------------------------------*)
(*                                   SLICES                                  *)
(*---------------------------------------------------------------------------*)
(*
   Une {\em tranche} de $l = \langle x_1,\dots x_n\rangle$ est une sous-liste
   de $l$ de la forme $\langle x_i, \dots x_{j}$, o\`u $1 \leq i \leq j\leq n$.
 *)


(* if list = pre@suf, return Some suf. otherwise, return None *)
  let rec cut_prefix (slice : 'a list) (list : 'a list) : 'a list option =
    match (slice, list) with
    | ([], droite) -> Some droite
    (*Si la liste "slice" est vide, cela signifie que nous avons parcouru tous les éléments dont nous avons besoin de faire correspondre dans la liste "list". Dans ce cas, nous retournons Some droite car "droite" contient les éléments restants de la liste "list".*)
    | (x::gauche, y::droite) when x=y -> cut_prefix gauche droite
    (*Si le premier élément de "slice" correspond au premier élément de "list", nous avançons d'un élément dans "slice" et "list" en appelant récursivement la fonction cut_prefix avec "gauche" (slice sans le premier élément) et "droite" (list sans le premier élément).*)
    | _ -> None
    (*Si aucun des cas ci-dessus ne s'applique, cela signifie qu'il n'y a pas de correspondance entre "slice" et "list". Dans ce cas, nous retournons None.*)
  

(*
  cut_prefix [1; 2; 3] [1; 2; 3; 4] = Some [4]
  cut_prefix [1; 2; 3; 4] [1; 2; 3; 4] = Some []
  cut_prefix [1; 2; 0] [1; 2; 3; 4] = None
 *)


(* return the prefix and the suffix of the first occurrence of a slice,
   or None if this occurrence does not exist.
*)
  let rec starts_with prefix list = (* fonction auxiliaire pour vérifier si on retrouve le début d'un certain prefix dans une liste*)
    match (prefix, list) with
    | ([], _) -> true
    | (x :: pre, y :: rest) when x = y -> starts_with pre rest
    | _ -> false
 
  let rec separate x y = match (x,y) with (*fonction auxiliaire pour séparer un certain prefix d'une liste*)
    | ([],t)-> t
    | (x :: pre, y :: rest) when x = y -> separate pre rest
    | _ -> []
  

let first_occ (slice : 'a list) (list : 'a list)
    : ('a list * 'a list) option =
    let rec find_first_occurrence before list =
      match list with
      | [] -> None
      | _ when starts_with slice list ->
        Some (before, (separate slice list))
      | x :: rest -> find_first_occurrence (before @ [x]) rest
    in
    match list with
    | [] -> if slice = [] then Some([],[]) else  None
    | _ -> find_first_occurrence [] list
    
(*
  first_occ [1; 2] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([1; 1], [3; 4; 1; 2])
  first_occ [1; 1] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([], [1; 2; 3; 4; 1; 2])
  first_occ [1; 3] [1; 1; 1; 2; 3; 4; 1; 2] = None
 *)


let rec slices_between
          (start : 'a list) (stop : 'a list) (list : 'a list) : 'a list list =
          let rec aux start stop list res=
          (* Recherche du premier occurrence de 'start' dans la liste *)
          match first_occ start list with
          |None-> res
          |Some (prefixe,suffixe)-> match first_occ stop suffixe with    (* Si 'start' est trouvé, recherche du premier occurrence de 'stop' dans le suffixe *)
                         |None->res (* Si 'stop' n'est pas trouvé, renvoyer le résultat actuel *)
                         |Some (suprefixe,susuffixe)-> aux start stop susuffixe (suprefixe::res) in (* Appel récursif avec le reste de la liste après 'stop' et la nouvelle tranche trouvée *)
                         List.rev(aux start stop list []) (* Appel de la fonction auxiliaire avec la liste d'origine et une liste vide comme accumulateur initial *)
                         
        
(*
  slices_between [1; 1] [1; 2] [1; 1; 1; 1; 2; 1; 3; 1; 2] = [[1]]
 *)

let cut_genes (dna : dna) : (dna list) =
  slices_between [A;T;G] [T;A;A] dna

(*---------------------------------------------------------------------------*)
(*                          CONSENSUS SEQUENCES                              *)
(*---------------------------------------------------------------------------*)


type 'a consensus = Full of 'a | Partial of 'a * int | No_consensus

(* return (Full a) if all elements of the list are equal to a,
   (Partial (a, n)) if a is the only element of the list with the
   greatest number of occurrences and this number is equal to n,
   No_consensus otherwise. the list must be non-empty *)
  
  (* Fonction récursive pour trouver ou mettre à jour le nombre d'occurrences d'un élément dans une liste de tuples *)
let rec trouver_occurrence element liste_tuples =
  match liste_tuples with
  | [] -> []  (* L'élément n'a pas été vu, donc le nombre d'occurrences est 0 *)
  | (e, occurrences) :: tl ->
    if e = element then  (* On croise l'élément dans notre liste *)
      (e, occurrences + 1) :: tl
    else  (* On ne croise pas l'élément, on continue notre recherche *)
      [(e, occurrences)] @ trouver_occurrence element tl    

(* Fonction récursive pour créer une liste de tuples représentant les occurrences des éléments dans une liste *)
let rec creation li res =
  match li with 
  | [] -> res
  | x :: tl -> 
    (* Si on n'a pas trouvé l'élément dans res, on l'ajoute avec une occurrence de 1 *)
    if (trouver_occurrence x res) = [] || (trouver_occurrence x res = res) then 
      let r = [(x, 1)] in
      creation tl (res @ r)
    else 
      let f = trouver_occurrence x res in 
      creation tl f

(* Fonction principale pour déterminer le consensus dans une liste *)
let consensus l =
  if l = [] then No_consensus  (* Si la liste est vide, il n'y a pas de consensus *)
  else
    let v = List.hd l in  (* Valeur initiale pour le consensus *)
    let total = creation l [] in  (* Création d'une liste d'occurrences *)
    let rec verif t valeur nb =
      match t with
      | [] -> if nb = 0 then No_consensus else Partial (valeur, nb)
      | (x, m) :: tl ->
          let val_actuel = valeur in
          let max_actuel = nb in 
          if List.length total = 1 then Full x  (* Si la liste d'occurrences a une seule valeur, c'est un consensus complet *)
          else  
            if nb = 0 then verif tl x m 
            else 
              if m > max_actuel then verif tl x m
              else if m = max_actuel then No_consensus
              else verif tl val_actuel max_actuel
    in
    verif total v 0


(*
   consensus [1; 1; 1; 1] = Full 1
   consensus [1; 1; 1; 2] = Partial (1, 3)
   consensus [1; 1; 2; 2] = No_consensus
 *)

(* return the consensus sequence of a list of sequence : for each position
   in the elements of ll, compute the consensus  of the set of values at this
   position  in the sequences. the lists must be of same length. if all lists
   are empty, return the empty sequence.
 *)

 (* Fonction pour transposer une matrice (liste de listes) *)
(* Fonction pour transposer une matrice (liste de listes) *)
let transpose matrix =
  (* Correspondance de motifs pour les différents cas de la matrice *)
  match matrix with
  | [] -> [] (* Si la matrice est vide, la matrice transposée est également vide *)
  | [] :: _ -> [] (* Si la première ligne de la matrice est vide, la matrice transposée est également vide *)
  | _ ->
    let rec transpose_aux acc matrix =
      (* Correspondance de motifs pour gérer les différents cas lors de la transposition *)
      match matrix with
      | [] :: _ -> List.rev acc (* Si la première colonne de la matrice est vide, renvoyer la matrice transposée (acc) *)
      | m ->
        (* Extraire les têtes de chaque liste (colonne) dans la matrice *)
        let head_column = List.map List.hd m in
        (* Extraire les queues de chaque liste (reste de la colonne) dans la matrice *)
        let tail_columns = List.map List.tl m in
        (* Appel récursif avec les queues pour traiter les colonnes suivantes *)
        transpose_aux (head_column :: acc) tail_columns
    in
    (* Appel initial de la fonction auxiliaire avec une liste vide comme accumulateur *)
    transpose_aux [] matrix
;;



let consensus_sequence (ll : 'a list list) : 'a consensus list =
  let t = transpose ll in
 List.map consensus t



(*
 consensus_sequence [[1; 1; 1; 1];
                     [1; 1; 1; 2];
                     [1; 1; 2; 2];
                     [1; 2; 2; 2]]
 = [Full 1; Partial (1, 3); No_consensus; Partial (2, 3)]
 *)
