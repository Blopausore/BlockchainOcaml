open Big;;
open Sha256;;
open Merkle;;
open Blockchain;;


(*VARIABLES*)
let ecart = 0;; (*21 blocks entre deux mise a jour*)

(* Initialise des fichiers *)

let path_blockchain = "/home/blopausore/Documents/Tipe/Blockchain/";;
let path_etats = "/home/blopausore/Documents/Tipe/Etats/";;
let path_acteurs = "/home/blopausore/Documents/Tipe/Acteurs/";;
let path_admins =  "/home/blopausore/Documents/Tipe/Admins/";;
let path_flux =  "/home/blopausore/Documents/Tipe/Flux/";;

(*FONCTIONS RANDOMS*)

let rec random_num = function
    |0 -> ""
    |n -> (string_of_int (Random.int 10))^(random_num (n-1));;



(*FONCTIONS AUXILIAIRES*)
let print_last_block blockchain =  
    let block = List.hd blockchain in
    
    print_string "hash 0 : ";
    big_to_hex block.hash0;
    
    print_string "racine de merkle :";
    big_to_hex block.tx;
    
    print_string "hash 1 : ";
    big_to_hex block.hash1;;
    
let write canal s = output canal (Bytes.of_string s) 0 (String.length s);;

let rec join ls sep = match ls with
    |[] -> ""
    |h::[] -> h
    |h::ts -> h ^ sep ^ (join ts sep);;  


let path_individu id = path_etats ^ id ^ ".txt";;


(*INITIALISATION*)

let initialise_individus n = 
    let individus = Array.make n "" in
    for i = 0 to (n-1) do
        let id = (random_num 16) in 
        let individu = open_out (path_individu id) in
        write individu "0;0;0";
        individus.(i) <- id;
        close_out individu;
    done;
    individus;;


let initialise_acteurs n = 
    let acteurs = Array.make n "" in
    for i = 0 to (n-1) do
        let id = (random_num 16) in 
        let acteur = open_out (path_acteurs^id^".txt") in
        write acteur "0;0";
        close_out acteur;
        acteurs.(i) <- id;
    done;
    acteurs;;
    
        
(*update blockchain*)
let incr_str s =
    string_of_int ( (int_of_string s) +1);;
    
let update blockchain id_ind id_acteur id_res =
(*Genere une update dans le potentiel prochain bloc*)
    let individu = open_in (path_individu id_ind) in
    let (etat,id) = match (String.split_on_char ';' (input_line individu)) with
        |e::i::_ -> e,i
        |_ -> failwith "Error of format"
    in
    close_in individu;
    
    let id_block = (string_of_int (id_blockchain blockchain )) in
    
    let new_etat = (incr_str etat) in
    
    let individu = open_out (path_individu id_ind) in
    write individu (join [new_etat ;id_block] ";");
    close_out individu;
    
    let acteur = open_out (path_acteurs^id_acteur^".txt") in
    write acteur (join [id_res; id_block;"\n"] ";");
    close_out acteur;
    
    
    let flux_write = open_out (path_flux ^ (join [id_ind;id_acteur;".txt"] "_")) in
    write flux_write (join [id_ind;new_etat;id_acteur;id_res] ";");
    close_out flux_write;;
    

let clear_files = function () ->
(*Nettoi les dossiers employe durant les simulations *)
    clear_dir path_blockchain;
    clear_dir path_acteurs;
    clear_dir path_flux;
    clear_dir path_etats;;
    
(*SIMULATION*)
        
   



