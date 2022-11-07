
(* FONCTION DE HASHAGE SHA256 *)
open Big;;

let p32 = pui2 32;;
let h0 = [| 1779033703; 3144134277; 1013904242; 2773480762; 1359893119; 2600822924; 528734635; 1541459225|];;
let k =
[| 1116352408; 1899447441; 3049323471; 3921009573; 961987163; 1508970993; 2453635748; 2870763221;
 3624381080; 310598401; 607225278; 1426881987; 1925078388; 2162078206; 2614888103; 3248222580;
  3835390401; 4022224774; 264347078; 604807628; 770255983; 1249150122; 1555081692; 1996064986;
   2554220882; 2821834349; 2952996808; 3210313671; 3336571891; 3584528711; 113926993; 338241895;
    666307205; 773529912; 1294757372; 1396182291; 1695183700; 1986661051; 2177026350; 2456956037;
     2730485921; 2820302411; 3259730800; 3345764771; 3516065817; 3600352804; 4094571909; 275423344;
      430227734; 506948616; 659060556; 883997877; 958139571; 1322822218; 1537002063; 1747873779;
 1955562222; 2024104815; 2227730452; 2361852424; 2428436474; 2756734187; 3204031479; 3329325298|];;

let (%) = fun x n -> x land (n -1);;

let (>>) = fun x n -> ( (x lsr n) lor (x lsl (32 - n)  % p32 ) )   ;;




(*Creer un big int de deux cases de l *)
let len_end_big l =
	let d = (decoupage l 32).big in
	if List.length d = 2 then d
	else 0::d;;

(* Creer une liste de zerros de la taille en entree *)
let rec zeros end_list = function
	|0 -> end_list
	|k -> 0 :: zeros end_list (k-1) ;;

(* inverse la list l *)

let inverse l =
	let rec aux acc = function
		|[] -> acc
		|t::q -> aux (t::acc) q
	in aux [] l;;

(* Construction de Merkle-Damgard*)
(* Resistance aux attaques de preimage *)
let pre_traitement b =
	let l = len_big_int b in
	let b_32 = base ( ajout_big b (1 lsl (b.taille-1)) ) 32 in
	let ln = List.length b_32.big in
	let i = ref 0 in
	while (!i * 16 -ln -2) < 0
		do incr i done ;
	let k = ( !i  * 16 - ln - 2 ) in
	let rec remplissage = function
		|[] -> zeros (len_end_big l) k
		|t :: q -> t :: remplissage q
	in {taille = 32 ; big = remplissage b_32.big };;

(* Generateur de m : b un big_int de taille 32, son format est donc int_list *)
let m_generateur b =
	let l = List.length b in
	let m = Array.make_matrix ( l/16 ) 16 0 in
	let rec aux b i k = match b,k with
		|[] , _ -> ()
		|t::q, 16 -> aux (t::q) (i+1) 0
		|t::q, k -> m.(i).(k) <- t; aux q i (k+1)
	in aux b 0 0; m;;


let  sig0 x = ( x >> 7) lxor (x >> 18) lxor (x lsr 3)
and sig1 x  = (x >> 17) lxor (x >> 19) lxor (x lsr 10)
and epsi0 x  =  (x >> 2) lxor (x >> 13) lxor (x >> 22)
and epsi1 x = (x >> 6) lxor (x >> 11) lxor (x >> 25) ;;


let w_generateur m =
	let w = Array.make 64 0 in
	for t = 0 to 15 do
		w.(t) <- m.(t)
	done;
	for t= 16 to 63 do
		w.(t) <- ( w.(t-16) + sig0(w.(t-15)) + w.(t-7) + sig1(w.(t-2)) ) % p32;
	done;
	w;;

let array_to_list a =
	let rec aux acc = function
		|(-1) -> acc
		|k -> aux (a.(k) :: acc) (k-1)
	in aux [] (Array.length a -1);;

(* Choice : les bits de x determine quelle bit va être choisi entre ceux de y et z*)
let ch x y z = (x land y) lxor ( (lnot x) land z);;

(* Majority : Le bit choisis et celui en majorité *)
let maj x y z = (x land y) lxor (x land z) lxor (y land z);;

let abcdefgh_generateur hash w =
	let  a = ref hash.(0) 	and b = ref hash.(1)	and c= ref hash.(2) 	and d= ref hash.(3)
	and e= ref hash.(4) and f = ref hash.(5) and g= ref hash.(6) and h = ref hash.(7) in
	for t = 0 to 63 do
		let t1 = (!h + epsi1(!e) + (ch !e !f !g) + k.(t) + w.(t)) % p32
		and t2 = ( epsi0(!a) + (maj !a !b !c) ) % p32 in
		h := !g ;
		g := !f;
		f := !e;
		e := (!d + t1) % p32;
		d := !c;
		c := !b;
		b := !a;
		a := (t1 + t2) % p32;
	done;
	[| !a ; !b ; !c ; !d ;!e ; !f ; !g ; !h |];;

let print_array_bin a =
	for i = 0 to (Array.length a -1) do
		if (i mod 2) = 0 then print_newline() ;
		int_to_bin a.(i) 32; print_string " "
	done;;

 let sha256 b =
	let m = m_generateur ((pre_traitement b).big) in
	let n = Array.length m in
	let hash = Array.make 8 0 in
	for i = 0 to 7 do hash.(i) <- h0.(i) done;

	for i = 1 to n do
		let w = w_generateur m.(i-1) in

		let abcdefgh = abcdefgh_generateur hash w in
		for j = 0 to 7 do
			hash.(j) <- ( (abcdefgh.(j) + hash.(j)) % p32)
		done;
	done;
	{taille = 32 ; big = array_to_list hash};;
(*
let sha = sha256 sys.argv.(1);;
let _ = big_to_hex sha;;
*)
