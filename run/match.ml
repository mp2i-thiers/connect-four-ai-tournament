(*************** Logique de la grille **************)

let red = "\x1b[41m"
let blue = "\x1b[44m"
let reset = "\x1b[0m"

let afficher grille =
  let ligne i = 
    Printf.printf "%d |" i;
    for j = 0 to 6 do
      match grille.(j).(i) with
        | None -> Printf.printf "   |"
        | Some Tournoi.J1 -> Printf.printf "%s X %s|" red reset
        | Some Tournoi.J2 -> Printf.printf "%s O %s|" blue reset
    done;
    Printf.printf "\n"
  in
  (* Indication des numéros de colonnes *)
  Printf.printf "\n  ";
  for i = 0 to 6 do
    Printf.printf "  %d " i
  done;
  (* Lignes *)
  Printf.printf "\n";
  for i = 5 downto 0 do
    ligne i
  done;
  (* Socle *)
  Printf.printf "==+";
  for _ = 0 to 6 do Printf.printf "===+" done;
  Printf.printf "\n"


let adversaire j = match j with
 | Tournoi.J1 -> Tournoi.J2
 | Tournoi.J2 -> Tournoi.J1

let init () =
  let grille = Array.make 7 [||] in
  for i = 0 to 6 do
    grille.(i) <- Array.make 6 None
  done;
  grille

exception Break

let complet grille = 
    try 
        Array.iter (fun l -> if l.(5) = None then raise Break) grille;
        true
    with Break -> false

let peut_jouer grille colonne = grille.(colonne).(5) = None

let jouer grille colonne j = 
    assert (peut_jouer grille colonne);
    try 
        for i = 0 to 5 do
            if grille.(colonne).(i) = None then begin
                grille.(colonne).(i) <- Some j;
                raise Break
            end
        done
    with Break -> ()


(* Je sais le code est pas beau... *)
exception Victoire

let verif_colonnes e joueur =
    for j = 0 to 6 do
        for k = 0 to 2 do
            let r = ref true in
            for i = k to k + 3 do
                if e.(j).(i) <> Some joueur then r := false
            done;
            if !r then raise Victoire
        done
    done

let verif_lignes e joueur =
    for i = 0 to 5 do
        for k = 0 to 3 do
            let r = ref true in
            for j = k to k + 3 do
                if e.(j).(i) <> Some joueur then r := false
            done;
            if !r then raise Victoire
        done
    done

let verif_diagonales_montantes e joueur =
    for i = 0 to 2 do
        for j = 0 to 3 do
            let r = ref true in
            for k = 0 to 3 do
                if e.(j+k).(i+k) <> Some joueur then r := false
            done;
            if !r then raise Victoire
        done
    done

let verif_diagonales_descendantes e joueur =
    for i = 3 to 5 do
        for j = 0 to 3 do
            let r = ref true in
            for k = 0 to 3 do
                if e.(j+k).(i-k) <> Some joueur then r := false
            done;
            if !r then raise Victoire
        done
    done
    
let victoire e joueur = 
    try
        verif_colonnes e joueur;
        verif_lignes e joueur;
        verif_diagonales_montantes e joueur;
        verif_diagonales_descendantes e joueur;
        false
    with Victoire -> true


(*************** Boucle de jeu principale **************)

(* Ia1 et Ia2 modules des ia des deux concurrents *)

let j_to_string j = match j with
| Tournoi.J1 -> "J1"
| Tournoi.J2 -> "J2"

(* lance l'ia avec un timeout de max_time secondes *)
let run_ia ia e j depth name max_time =
    try 
        Core.Signal.Expert.handle Core.Signal.alrm (
            fun (_:Core.Signal.t) -> Core.printf "timeout\n"; exit 1);
        ignore (Unix.alarm max_time : int);
        ia e j depth
    with _ -> failwith ("erreur soulevée par " ^ name)

let copy_matrix e = 
    let e' = init () in
    Array.iteri (fun i xi ->
        Array.iteri (fun j x -> e'.(i).(j) <- x) xi
    ) e;
    e'

let run_match d1 d2 t =
    assert (d1 >= 0 && d2 >= 0);
    Random.self_init ();
    let e = init () in
    let m = ref [] in
    let j = ref Tournoi.J1 in
    while (not (complet e) && not (victoire e Tournoi.J1) && not (victoire e Tournoi.J2)) do
        let coup = match !j with
        | Tournoi.J1 -> run_ia Ia1.ia (copy_matrix e) !j d1 "Ia1" t
        | Tournoi.J2 -> run_ia Ia2.ia (copy_matrix e) !j d2 "Ia2" t
        in 
        m := string_of_int(coup) :: !m;
        afficher e;
        flush stdout;
        try 
            jouer e coup !j;
            j := adversaire !j;
        with _ -> 
                failwith ("Coup invalide donné par " ^ (j_to_string !j))
    done;

    if (victoire e Tournoi.J1)
    then m := "v1" :: !m
    else begin
        if (victoire e Tournoi.J2) 
        then m := "v2" :: !m
        else m := "null" :: !m
    end;
    List.rev !m

let bench_ia ia name d t e=
    ignore(run_ia ia e Tournoi.J1 d name t)

let print_coups m = 
    List.iter (fun x -> Printf.printf "%s," x) m;
    Printf.printf "\n"

let set1 = init ()
let set2 = 
    let e = init () in
    jouer e 0 Tournoi.J1;
    jouer e 5 Tournoi.J2;
    jouer e 5 Tournoi.J1;
    jouer e 2 Tournoi.J2;
    jouer e 4 Tournoi.J1;
    e
let set3 = 
    let e = init () in
    jouer e 2 Tournoi.J1;
    jouer e 3 Tournoi.J2;
    jouer e 6 Tournoi.J1;
    jouer e 0 Tournoi.J2;
    jouer e 4 Tournoi.J1;
    jouer e 1 Tournoi.J2;
    e
let set_rnd = 
    let e = init () in
    jouer e (Random.int 7) Tournoi.J1;
    jouer e (Random.int 7) Tournoi.J2;
    jouer e (Random.int 7) Tournoi.J1;
    jouer e (Random.int 7) Tournoi.J2;
    jouer e (Random.int 7) Tournoi.J1;
    jouer e (Random.int 7) Tournoi.J2;
    e

let sets = [| set1; set2; set3; set_rnd |]

let _ = 
    Random.self_init ();
    if Array.length Sys.argv = 6 && Sys.argv.(1) = "bench" then begin
        let d = int_of_string (Sys.argv.(3)) in
        let t = int_of_string (Sys.argv.(4)) in
        let set = int_of_string (Sys.argv.(5)) in
        if Sys.argv.(2) = "1" then (bench_ia Ia1.ia "ia1" d t sets.(set); exit 0)
        else begin 
            if Sys.argv.(2) = "2" then (bench_ia Ia2.ia "ia2" d t sets.(set); exit 0)
            else failwith "joueur en argument invalide"
        end
    end
    else if Array.length Sys.argv = 5 && Sys.argv.(1) = "run" then begin
            let d1 = int_of_string Sys.argv.(2) in
            let d2 = int_of_string Sys.argv.(3) in
            let t = int_of_string Sys.argv.(4) in
            let m = run_match d1 d2 t in
            print_coups m
    end
    else failwith "arguments invalide"
