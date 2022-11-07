open Big;;
open Sha256;;
open Merkle;;
open Blockchain;;
open Simulation;;

(*Simule une tentative d'update force*)
let _ = 
    let blockchain = ref [block0] in
    print_endline "SIMULATION BLOCKCHAIN";

    clear_files ();
    print_endline "Fichier clear.";

    (* SIMULATION 1 *)

    let individus = initialise_individus 100 in
    print_endline "Initialisation des individus : done";

    let acteurs = initialise_individus 10 in
    print_endline "Initialisation des acteurs : done";

    update !blockchain individus.(0) acteurs.(0) "0";
    update !blockchain individus.(1) acteurs.(0) "0";
    update !blockchain individus.(2) acteurs.(1) "0";
    update !blockchain individus.(3) acteurs.(1) "0";
    update !blockchain individus.(4) acteurs.(2) "0";
    
    print_endline "Fin de l'intialisation";
    print_endline "Etape suivante : creation du block 1";
    print_endline "Appuyez sur entier pour continuer :";
    let _ = read_int() in
    
    blockchain := (new_block !blockchain )::!blockchain;
    print_last_block !blockchain;
    
    let fichier_malveillant = open_out (path_blockchain^"block_2/12121212_34343434.txt") in
    write fichier_malveillant "12121212;3;34343434;0";
    close_out fichier_malveillant;
    
    print_string "Nouvelle racine de Merkle : ";
    big_to_hex (merkle (path_block 1));
    if not (verification !blockchain) 
        then print_endline "Un changement a ete detecte";
    
    print_endline "FIN DE LA SIMULATION 2";;



    


    
    
    
        

