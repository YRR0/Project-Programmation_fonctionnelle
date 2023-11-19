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
  let taille=String.length str in
  let rec aux index acc=
    if index>=taille then acc 
    else aux (index+1) (str.[index] :: acc) in
    let x=aux 0 [] in List.rev x


(* conversions *)
let base_of_char (cgit : char) : base = match cgit with
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
let rec cut_prefix (slice : 'a list) (list : 'a list) : 'a list option = match (slice, list) with
| ([],droite) -> Some droite
|(x::gauche, y::droite) when x=y -> cut_prefix gauche droite
|_-> None


(*
  cut_prefix [1; 2; 3] [1; 2; 3; 4] = Some [4]
  cut_prefix [1; 2; 3; 4] [1; 2; 3; 4] = Some []
  cut_prefix [1; 2; 0] [1; 2; 3; 4] = None
 *)


(* return the prefix and the suffix of the first occurrence of a slice,
   or None if this occurrence does not exist.
*)
let first_occ (slice : 'a list) (list : 'a list) : ('a list * 'a list) option =
  match cut_prefix slice list with
  |Some suffix->Some([],suffix)
  |None->let rec aux avant apres =
            match apres with
            |[]->None
            |hd::ls-> match cut_prefix slice apres with
                      |Some suffix-> Some (avant ,suffix)
                      |None-> aux (avant @ [hd] ) ls 
            in
            aux [] list


      (*
      let first_occ (slice : 'a list) (list : 'a list) : ('a list * 'a list) option =
        let rec aux avant apres =
          match apres with
          | [] -> None
          | hd::tl -> 
            match cut_prefix slice tl with
            | Some suffix -> Some (avant, suffix)
            | None -> aux (avant @ [hd]) tl
        in
        aux [] list
        *)



(*
  first_occ [1; 2] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([1; 1], [3; 4; 1; 2])
  first_occ [1; 1] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([], [1; 2; 3; 4; 1; 2])
  first_occ [1; 3] [1; 1; 1; 2; 3; 4; 1; 2] = None
 *)

let rec slices_between
          (start : 'a list) (stop : 'a list) (list : 'a list) : 'a list list =
          let rec aux start stop list res=
          match first_occ start list with
          |None->res
          |Some (prefixe,suffixe)-> match first_occ stop suffixe with
                         |None->res
                         |Some (suprefixe,susuffixe)-> aux start stop susuffixe (suprefixe::res) in
                         List.rev(aux start stop list [])

        
  

(*
  slices_between [1; 1] [1; 2] [1; 1; 1; 1; 2; 1; 3; 1; 2] = [[1]; []; [2; 1; 3]]
 *)

let cut_genes (dna : dna) : (dna list) =
  slices_between [A; T; G] [T; A; A] dna

(*---------------------------------------------------------------------------*)
(*                          CONSENSUS SEQUENCES                              *)
(*---------------------------------------------------------------------------*)


type 'a consensus = Full of 'a | Partial of 'a * int | No_consensus

(*type 'a compteur = { element : 'a; nb : int }*)


let maximum4 a c g t nbA nbC nbG nbT=
  if nbC=0 && nbG=0 && nbT=0 then Full a
  else if nbA>nbC && nbA>nbG && nbA>nbT then Partial (a,nbA)
  else if nbC>nbA && nbC>nbG && nbC>nbT then Partial (c,nbC)
  else if nbG>nbA && nbG>nbC && nbG>nbT then Partial (g,nbG)
  else if nbT>nbA && nbT>nbC && nbT>nbG then Partial (t,nbT)
  else No_consensus


let rec partial4 a c g t nbA nbC nbG nbT list=
  match list with
  |[]->  maximum4 a c g t nbA nbC nbG nbT 
  |hd::ls-> if hd=a then partial4 a c g t (nbA+1) nbC nbG nbT ls
            else if hd=c then partial4 a c g t nbA (nbC+1) nbG nbT ls 
            else if hd=g then partial4 a c g t nbA nbC (nbG+1) nbT ls 
            else if hd=t then partial4 a c g t nbA nbC nbG (nbT+1) ls
            else No_consensus

let rec partial3 a c g t nbA nbC nbG nbT list=
  match list with
  |[]->  maximum4 a c g t nbA nbC nbG nbT 
  |hd::ls-> if hd=a then partial3 a c g t (nbA+1) nbC nbG nbT ls
            else if hd=c then partial3 a c g t nbA (nbC+1) nbG nbT ls 
            else if hd=g then partial3 a c g t nbA nbC (nbG+1) nbT ls 
            else partial4 a c g hd nbA nbC nbG (nbT+1) ls

let rec partial2 a c g t nbA nbC nbG nbT list=
  match list with
  |[]-> maximum4 a c g t nbA nbC nbG nbT 
  |hd::ls-> if hd=a then partial2 a c g t (nbA+1) nbC nbG nbT ls
            else if hd=c then partial2 a c g t nbA (nbC+1) nbG nbT ls 
            else partial3 a c hd t nbA nbC (nbG+1) nbT ls

let rec partial1 a c g t nbA nbC nbG nbT list=
  match list with
  |[]-> maximum4 a c g t nbA nbC nbG nbT 
  |hd::ls-> if hd=a then partial1 a c g t (nbA+1) nbC nbG nbT ls
            else partial2 a hd g t nbA (nbC+1) nbG nbT ls  
     
let consensus (list : 'a list) : 'a consensus =          
  match list with
  |[]->No_consensus
  |hd:: ls-> partial1 hd hd hd hd  1 0 0 0 ls


let consensus_sequence (ll : 'a list list) : 'a consensus list =
  failwith "A faire"

(*let consensus_sequence (ll : 'a list list) : 'a consensus list =
  (*let rec aux ll acc=
    match ll with 
    |[]->acc
    |hd :: ls-> let val=consensus *)
    failwith "A faire"*)



(*
 consensus_sequence [[1; 1; 1; 1];
                     [1; 1; 1; 2];
                     [1; 1; 2; 2];
                     [1; 2; 2; 2]]
 = [Full 1; Partial (1, 3); No_consensus; Partial (2, 3)]

 consensus_sequence [[]; []; []] = []
 *)
