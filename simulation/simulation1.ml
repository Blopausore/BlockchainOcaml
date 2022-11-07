open Big;;
open Sha256;;
open Merkle;;
open Blockchain;;
open Simulation;;

(*Simule un fonctionnement normal*)
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

    update !blockchain individus.(0) acteurs.(0) "0";
    update !blockchain individus.(2) acteurs.(1) "0";
    update !blockchain individus.(12) acteurs.(7) "0";
    update !blockchain individus.(13) acteurs.(7) "0";
    update !blockchain individus.(14) acteurs.(3) "0";
        
    print_endline"Etape suivante : creation du block 2";
    print_endline "Appuyez sur entier pour continuer :";
    let _ = read_int() in

    blockchain := (new_block !blockchain )::!blockchain;
    print_last_block !blockchain;

    update !blockchain individus.(23) acteurs.(7) "0";
    update !blockchain individus.(89) acteurs.(8) "0";
    update !blockchain individus.(2) acteurs.(1) "1";
    update !blockchain individus.(0) acteurs.(0) "0";
    update !blockchain individus.(4) acteurs.(2) "0";
    


    print_endline "Etape suivante : creation du block 3";
    print_endline "Appuyez sur entier pour continuer :";
    let _ = read_int() in
    
    blockchain := (new_block !blockchain )::!blockchain;
    print_last_block !blockchain ;
    
    print_endline "FIN DE LA SIMULATION 1";;



    


    
    
    
        

