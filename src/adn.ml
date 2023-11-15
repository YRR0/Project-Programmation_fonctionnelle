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
  failwith "A faire"

(*
  slices_between [1; 1] [1; 2] [1; 1; 1; 1; 2; 1; 3; 1; 2] = [[1]; []; [2; 1; 3]]
 *)

let cut_genes (dna : dna) : (dna list) =
  failwith "A faire"

(*---------------------------------------------------------------------------*)
(*                          CONSENSUS SEQUENCES                              *)
(*---------------------------------------------------------------------------*)


type 'a consensus = Full of 'a | Partial of 'a * int | No_consensus

(* return (Full a) if all elements of the list are equal to a,
   (Partial (a, n)) if a is the only element of the list with the
   greatest number of occurrences and this number is equal to n,
   No_consensus otherwise. *)

   (*
let res a c g t types =
    if (a!=0&&c=0&&g=0&&t=0) then Full (List.nth types 0)
    else if (a=0&&c!=0&&g=0&&t=0) then Full (List.nth types 1)
    else if (a=0&&c=0&&g!=0&&t=0) then Full (List.nth types 2)
    else if (a=0&&c=0&&g=0&&t!=0) then Full (List.nth types 3)
    else  
      let x= max a c in
      let y= max g x in
      let maximum= max y t in
      if (a=maximum && c!=maximum && g!=maximum && t!=maximum) then Partial (List.nth types 0, maximum)
      else if (a!=maximum && c=maximum && g!=maximum && t!=maximum) then Partial (List.nth types 1, maximum)
      else if (a!=maximum && c!=maximum && g=maximum && t!=maximum) then Partial (List.nth types 2, maximum)
      else if (a!=maximum && c!=maximum && g!=maximum && t=maximum) then Partial (List.nth types 3, maximum)
      else No_consensus
*)
let consensus (list : 'a list) : 'a consensus =
  failwith "A faire"
  (*
let consensus1 (list : 'a list) : 'a consensus =
  let rec aux list a c g t =
    match list with
    |[]-> res a c g t 
    |hd :: ls-> match hd with 
                |A-> aux ls (a+1) c g t
                |C-> aux ls a (c+1) g t
                |G-> aux ls a c (g+1) t
                |T-> aux ls a c g (t+1)
                |_->No_consensus 
  in aux list 0 0 0 0*)

  (*

  let rec maj a c g t ctA ctC ctG ctT elem= 
   match elem with
   |a-> (ctA+1, ctC, ctG, ctT)
   |c-> (ctA, ctC+1, ctG, ctT)
   |g-> (ctA, ctC, ctG+1, ctT)
   |t-> (ctA, ctC, ctG, ctT+1)
   |_-> (ctA, ctC, ctG, ctT)


  let consensus2 (list : 'a list) : 'a consensus =
    let rec aux list a c g t ctA ctC ctG ctT=
      match list with
      |[]-> res a c g t 
      |hd ::ls-> aux ls a c g t (maj ctA ctC ctG ctT elem) in
      aux list a c g t 0 0 0 0
      *)


    

    (*let rec taille n list acc=
      let x=List.length list in
      let y=n-x in
      for i = 0 to x-1 do
        list@0 
      done; 
      list*)
    (*
    let rec sousListe list acc=
      match list with
      |[]->acc
      |hd :: ls-> if List.mem hd acc then sousListe ls acc else sousListe ls (hd :: acc)


    let consensus (list : 'a list) : 'a consensus =
      let ssListe= sousListe list [] in
      let taille=List.length ssListe in
      if taille=0 then begin res 0 0 0 0 end in
      if taille>0 then let first_elem =List.nth ssListe 0 
      if taille>1 then let var2=List.nth ssListe 1 in
      if taille>2 then let var3=List.nth ssListe 2 in
      if taille>3 then let var4=List.nth ssListe 3 in
      let rec auxConsensus list  a c g t =
        match list with
        |[]->res  a c g t
        |hd :: ls-> if hd= first_elem then auxConsensus ls (a+1) c g t 
               else if taille <2 then Full first_elem else if hd=var2  then auxConsensus ls a (c+1) g t
               else if taille <3 then auxConsensus ls ssListe a c g t else if hd=var3 then auxConsensus ls a c (g+1) t
               else if taille <4 then auxConsensus ls ssListe a c g t else if hd=var4 then auxConsensus ls a c g (t+1)
               else auxConsensus ls  a c g t in
               auxConsensus list 0 0 0 0*)
                
                
  

    

(*
   consensus [1; 1; 1; 1] = Full 1
   consensus [1; 1; 1; 2] = Partial (1, 3)
   consensus [1; 1; 2; 2] = No_consensus
 *)

(* return the consensus sequence of a list of sequences : for each position
   in the elements of ll, compute the consensus  of the set of values at this
   position  in the sequences. the lists must be of same length. if all lists
   are empty, return the empty sequence.
 *)
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
