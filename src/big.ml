(* MA CLASSE BIG_INT*)

let pui2 n = 1 lsl n;; (*Calcul 2**n*)


(* FONCTION CLASS BIG INT type 1 : [taille ; e1;e2;e3...]*)
type big_int = { taille : int ; big : int list } ;;

(*Decoupage <-> int_to_big *)
let decoupage n t =
(*int -> int -> big_int*)
	let rec aux acc = function
		|0 -> acc
		|n -> aux ((n - ((n lsr t) lsl t)) :: acc) (n lsr t )
	in {taille = t ; big = (aux [] n )};;


let string_to_big s =
(*string -> big*)
    let rec aux = function
        |(-1) -> []
        |k -> ( int_of_char s.[k] ) :: aux (k-1)
    in {taille = 8 ; big = aux (String.length s -1)};;



(*big_to_int *)

let big_to_int b =
(*bing_int -> int *)
	let rec aux acc = function
		|[] -> acc
		|n::q -> aux  (n+ (acc lsl b.taille ) ) q
	in aux 0 b.big ;;
	
(* Base *)

let base b n = (* Change la taille de b en taille n *)
(*big_int -> int -> big_int*)
	let t = b.taille in
	let rec aux d td l = match l,td with
		|[],_ -> if d=0 then [] else [d lsl td]
		|l,0 ->d :: aux 0 n l
		|di::q,td when td >= t ->aux (di+(d lsl t)) (td-t) q
		|di::q,td -> aux ( di lsr (t-td) + (d lsl (t-td) )) 0 (di - ( (di lsr (t-td) lsl (t-td) )) :: q)
	in {taille = n ; big = (aux 0 n b.big ) } ;;

(* big_to_hex *)

let hexa = [|"0";"1";"2";"3";"4";"5";"6";"7";"8";"9";"a";"b";"c";"d";"e";"f";"g";"h"|];;

let int_to_hex n k =(* n: entier a transformer ; k : combien de caractere la sortie doit contenir*)
(* int -> int -> unit *)
	let rec aux = function (* int -> list in*) (* Creer la liste d'hexadecimal*)
		|0 -> []
		|n ->  (n land 15) :: aux (n lsr 4)
	in
	let rec print_ k = function 
		|[] ->  if k > 0
			then (print_int 0 ; print_  (k-1) [])
			else ()
		|b::q -> print_  (k-1) q ; print_string hexa.(b)
	in
	print_ k (aux n);;


let big_to_hex b = 
(* big_int -> unit *)
	print_string "0x";
	let rec aux = function
		|[] -> ()
		|t :: q -> int_to_hex t (b.taille/4); aux q
	in aux b.big;print_newline();;


let int_to_hex_ n k= 
(* int -> int -> string *)
    let rec aux = function
	    |0 -> []
	    |n ->  (n land 15) :: aux (n lsr 4)
    in
    let rec print_ k = function
	    |[] ->  String.make k '0' 
	    |b::q -> (print_ (k-1) q) ^ hexa.(b)
    in
    print_ k (aux n);;

let big_to_string b = (* b doit etre de taille un diviseur de 4*)
(*big_int -> string*)
	let rec aux = function
		|[] -> ""
		|t :: q -> (int_to_hex_ t (b.taille/4)) ^ aux q
	in aux b.big;;


(* big_to_bin *)
let int_to_bin n k = 
(* int -> string *)
	let rec aux = function
		|0 -> []
		|n ->  (n land 1) :: aux (n lsr 1)
	in
	let rec print_ k ln = match ln,k with
		|[],0 -> ()
		|[],k ->print_int 0 ; print_ (k-1) []
		|b::q,k -> print_ (k-1) q;print_int b
	in
	print_ k (aux n);;

let big_to_bin b = 
(* big_int -> string *)
	print_string "0b";
	let rec aux = function
		|[] -> ()
		|t :: q -> int_to_bin t b.taille; aux q
	in aux b.big;print_newline();;

exception Not_On_Base;;

let (++) = fun b1 b2 ->
(*big_int -> big_int -> big_int*)
    if b1.taille != b2.taille then raise Not_On_Base
    else {taille = b1.taille ; big = b1.big @ b2.big} ;;


let (<=>) = fun b1 b2 ->
(*big_int -> big_int -> bool*)
    if b1.taille != b2.taille then raise Not_On_Base
    else begin
	let rec aux b1 b2 = match b1,b2 with
		|[],[] -> true
		|[],_ -> false
		|_,[] -> false
		|t1::q1,t2::q2 -> (t1 = t2) && aux q1 q2
	in aux b1.big b2.big; end;;

let ajout_big b n =
(*big_int -> n -> big_int *)
	let rec aux = function
		|[] -> (decoupage n b.taille).big
		|t :: q -> t :: aux q
	in {taille = b.taille ;big = (aux b.big)};;

let rec len = function (*Calcul sur combien de bit l'entier peut-il etre code*)
(* int -> int *)
	|0 -> 0
	|n -> 1 + len (n lsr 1);;

(*Retourne le nombre de 'cases' qui compose le nombre*)
let len_big_list b = List.length b.big;;

(*Retourne le nombre de bits sur lequel est code le nombre*)
let len_big_int b = 
(* big_int -> int *)
	(len_big_list b)*b.taille ;;


let string_to_big s =
(*string -> big_int*)
	let rec aux acc = function
		|(-1) -> acc
		|k -> aux (int_of_char(s.[k]) ::acc) (k-1)
	in {taille = 8 ; big = (aux [] (String.length s -1) ) };;
	
let incr_big b = (*augmente de 1 sans prendre compte d'erreur possible*)
(*big_int -> big_int*)
    {taille=b.taille ; big=[1]}++b;;


let print_big_bin b =
(*big_int -> unit*)
	let rec aux = function
		|[],_ -> ()
		|b,k when (k mod 2) = 0 -> print_newline() ; (aux (b,(k+1)))
		|t :: q,k -> int_to_bin t b.taille ;print_string " "; aux (q,(k+1))
	in aux (b.big,0);;

(* Random big_int : taille, nombre de cases *)

let random_big_int t n =
(*int -> int -> big_int*)
    let rec aux = function
        |0 -> []
        |k -> (Random.int (1 lsl t)) :: aux (k-1)
    in {taille = t ; big = (aux n)};;


