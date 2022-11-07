open Big;;
open Sha256;;
open Merkle;;
open Unix;;

type block =  {hash0 : big_int ; tx : big_int;id : big_int ; hash1 : big_int};;
	

let b1 = {taille = 32; big = [1]};;

let int_of_big b = List.hd b.big;; 
let first_hash = sha256 b1;;

let block0 = {hash0 = b1; tx = b1; hash1 = b1; id = b1};;

let path_blockchain = "/home/blopausore/Documents/Tipe/Blockchain/";;
let path_flux = "/home/blopausore/Documents/Tipe/Flux/";;

let path_block n = path_blockchain ^ "block_" ^ (string_of_int n);;
(* OPERATION SUR LA BLOCKCHAIN *)

let clear_dir path = List.iter Sys.remove (paths path);;

let rec last = function
    |[] -> ""
    |e::[] -> e
    |_::q -> last q;;

let flux_to_blockchain n = 
(*Deplace les fichiers contenu dans le dossier flux vers /blockchain/bloc_n/*)
    let new_file = path_block n in 
    
    if (Sys.file_exists new_file) 
        then clear_dir new_file 
        else Unix.mkdir new_file 0o755;
   
    let new_path f = new_file ^ "/"^f in
    let old_path f = path_flux ^ f in
    
    let rec aux = function
        |[] ->()
        |f::q -> let file = (last (String.split_on_char '/' f )) 
        in Unix.rename (old_path file) (new_path file);
        aux q;
    in aux (paths path_flux);;
        
let new_block blockchain =
(*Calcul le nouveau bloc *)
    let rtx = merkle path_flux in
    let aux blockchain = 
        match blockchain with
        |[] -> (b1,b1) 
        |{hash0 = h0;tx = rtx; id = ide; hash1 = h1}::_->
            (h1,ide)
    in 
    let (h,i) = aux blockchain in 
    let new_id = incr_big i in
    let h1 = sha256 ((rtx ++ new_id) ++ h) in
    flux_to_blockchain (List.length blockchain);
    {hash0 = h; tx = rtx; id = new_id ; hash1 = h1};;    
        
    
(* OPERATION SUR LES BLOCKS *)
(* bl : block ; dat : big_int list *)
    (* Verifie si deux blocks sont bien chaines *)
let ( >=> ) = fun bl1 bl2 -> bl1.hash1 <=> bl2.hash0;;

let id_blockchain blockchain = List.length blockchain;;

let verification_bloc {hash0 = h0;tx = rtx; id = ide; hash1 = h1} = 
    (h1 <=> sha256 ((rtx++ide)++h0) ) && rtx <=> (merkle (path_block (int_of_big ide))) ;;
    
        
let verification blockchain =
(* Verifie que la blockchain reste integre, que les donnees qu'elle garantit
n'ont pas change*)
    let last_b = List.hd blockchain in
    let rec aux h = function
        |[] -> true
        |b::q -> 
            (verification_bloc b) &&
            h <=> b.hash1 &&
            aux b.hash0 q
    in (verification_bloc last_b) && (aux last_b.hash0 (List.tl blockchain));;