(* MERKLE *)
open Big;;
open Sha256;;
open Unix;;

let files path = 
    let lignes = ref [] and texte = open_in path in
    try
        while true do
            lignes := input_line texte :: !lignes
        done; !lignes;
    with End_of_file ->
        close_in texte;
        !lignes;;
    
let lstring_to_big l = 
    let rec aux = function
        |[] -> ""
        |s::q -> s ^ (aux q)
    in string_to_big (aux l);;
    
let sha256_file path = 
    sha256 ( lstring_to_big (files path) );;


let extension s = match (String.split_on_char '.' s) with    
    |[] -> false
    |_::[] -> false
    |""::_ -> false
    |_ -> true;;
    

let paths path = 
(*string -> list string *)
(*Determine tous les chemins des fichiers dans le dossier de chemin path *)
    let fichiers = ref [] in
    let dossier = Unix.opendir path in
    try
        while true do 
            let file = (Unix.readdir dossier) in
            if (extension file) then 
            begin
                fichiers := ( path^"/"^ file ) :: !fichiers ;
            end
        done;
        !fichiers ;
    with End_of_file ->
        Unix.closedir dossier;
        !fichiers;;
        

let fbin n =
(*calcul la premiere puissance de 2 qui depasse n*)
    let k = ref 1 in
    while !k < n do k := !k * 2 done;
    !k;;
 
let add_zerros l k = 
    let rec aux = function
        |0 -> l
        |k -> {taille=32 ;big=[0]} :: aux (k-1)
    in aux k;;
    
let init_racine path = 
    let fichiers = paths path in 
    let n = List.length fichiers in
    
    let rec aux = function
        |[] -> []
        |f :: q -> sha256_file f :: aux q
    in add_zerros (aux fichiers) ((fbin n) -n);;
    
let print_big b = 
    let rec aux = function
        |[] -> ()
        |h::t -> print_int h;print_string " ";aux t
    in print_int b.taille;print_newline () ; aux b.big; print_newline();;
    
let rec parcours_largeur = function
    |[] -> []
    |_::[] -> []
    |b::d::q ->sha256 (b++d)  :: (parcours_largeur q);;
        
   
let rec print_list = function
    |[] -> ()
    |h::t -> big_to_hex h ; print_list t;;

let merkle path = 
    let rec ascension = function
        |[] -> {taille=0;big=[]}
        |r :: [] -> r
        |r :: q -> ascension (parcours_largeur (r :: q))
    in ascension (init_racine path) ;;
 
    
    
        
