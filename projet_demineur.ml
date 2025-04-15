open Graphics;;
open Random;;
open_graph " 600x700";;
set_window_title "DEMINEUR";;

Random.self_init ();;

type etat = Gagne | Perdu | EnCours;;

type case = Cachee of bool | Indice of int | Bombe | Vide;;

let ecrit_texte texte x y taille = 
    begin
    Graphics.set_text_size taille;
    let l_texte, h_texte = (text_size texte) in
    moveto (x-(l_texte/2)) (y-(h_texte/2));
    draw_string texte;
    end;;

let coordonnees_proportion x y = (int_of_float (x*.float_of_int (size_x ())), int_of_float (y*.float_of_int (size_y ())));;

let bombes_aleatoires () =
  let rec aux accu =
    if List.length accu = 15 then accu
    else
      let rand_x = (Random.int 16)
      and rand_y = (Random.int 16) in
      if List.mem (rand_x, rand_y) accu then aux accu else aux ((rand_x,rand_y)::accu)
  in aux [];;

let nombre_bombes_adjacentes x y etat_jeu =
  let k = ref 0 in
  for i = -1 to 1 do
    for j = -1 to 1 do
      try
        match (List.nth (List.nth etat_jeu (y+i)) (x+j)) with
        | Cachee true -> k := !k + 1
        | Bombe -> k := !k + 1
        | _ -> ()
      with
        | Invalid_argument _ -> ();
        | Failure a -> ();
    done;
  done;
  !k;;

let jeu_initial bombes =
  let rec cree_ligne accu y = function
  | 0 when (List.exists (fun e -> e = (0, y)) bombes) -> (Cachee true)::accu
  | 0 -> (Cachee false)::accu
  | k when (List.exists (fun e -> e = (k, y)) bombes) -> cree_ligne ((Cachee true)::accu) y (k-1)
  | k -> cree_ligne ((Cachee false)::accu) y (k-1)
  in
  let rec aux accu = function
  | 0 -> accu
  | k -> aux ((cree_ligne [] (k-1) 15)::accu) (k-1)
  in aux [] 16;;

let dessine_case (case : case) x y w h = match case with
| Cachee _ -> begin set_color (rgb 123 123 123); fill_rect (x+2) (y+2) (w-4) (h-4); end
| Indice k -> begin set_color black; moveto (x+(w/2)) (y+(h/2)); draw_string (string_of_int k); end
| Bombe -> begin set_color red; fill_rect (x+2) (y+2) (w-4) (h-4); end
| Vide -> begin set_color white; fill_rect (x+2) (y+2) (w-4) (h-4); end;;

let coordonnees_quadrillage () =
  let intervalle_x = int_of_float ((1. /. 16.) *. 0.76 *. float_of_int(size_x ()))
  and intervalle_y = int_of_float ((1. /. 16.) *. 0.69 *. float_of_int(size_y ())) in
  let diff_x = int_of_float ((0.76 *. float_of_int(size_x ())) -. (float_of_int (16*intervalle_x)))
  and diff_y = int_of_float ((0.69 *. float_of_int(size_y ())) -. (float_of_int (16*intervalle_y))) in
  let x_i, y_i = coordonnees_proportion 0.12 0.07 in
  let x_init = x_i + (diff_x / 2)
  and y_init = y_i + (diff_y / 2)
  in x_init, y_init, intervalle_x, intervalle_y;;

let dessine_quadrillage etat_jeu =
    set_color (rgb 123 123 123);
    let x_init, y_init, intervalle_x, intervalle_y = coordonnees_quadrillage () in
    for i = 0 to 16 do
      let x = x_init + (i * intervalle_x)
      and y = y_init + (i * intervalle_y)
      and h = y_init + 16 * intervalle_y
      and w = x_init + 16 * intervalle_x in begin
      moveto x y_init;
      lineto x h;
      moveto x_init y;
      lineto w y;
      end
    done;
    for i = 0 to 15 do
      for k = 0 to 15 do
        let x = x_init + (i * intervalle_x)
        and y = y_init + (k * intervalle_y)
        and etat_case = (List.nth (List.nth etat_jeu k) i) in
        dessine_case etat_case x y intervalle_x intervalle_y
      done;
    done;;

let charge_interface score etat_jeu t_init = begin
  clear_graph ();
  set_color black;
  let l_demin, h_demin = coordonnees_proportion 0.5 0.95 in
  ecrit_texte "DEMINEUR" l_demin h_demin 14;

  set_color (rgb 189 189 189);
  let x, y = coordonnees_proportion 0.10 0.05
  and w, h = coordonnees_proportion 0.8 0.85 in
  fill_rect x y w h;

  set_color white;
  let x, y = coordonnees_proportion 0.12 0.78
  and w, h = coordonnees_proportion 0.76 0.1 in
  fill_rect x y w h;

  set_color black;
  let t = (int_of_float(Unix.gettimeofday ()) - t_init)
  and l_s, h_s = coordonnees_proportion 0.8 0.83 in
  ecrit_texte (string_of_int t) l_s h_s 14;

  set_color black;
  let l_sc, h_sc = coordonnees_proportion 0.17 0.83 in
  ecrit_texte (String.cat "Score : " (string_of_int score)) l_sc h_sc 14;

  set_color white;
  let x, y = coordonnees_proportion 0.12 0.07
  and w, h = coordonnees_proportion 0.76 0.69 in
  fill_rect x y w h;

  dessine_quadrillage etat_jeu;
  end;;

let rec met_a_jour_clic et_jeu sc pos_x pos_y x_init y_init intervalle_x intervalle_y =
  if pos_x < x_init || pos_x > (x_init + (16* intervalle_x)) || pos_y < y_init || pos_y > (y_init + (16* intervalle_y)) then
    et_jeu, sc
  else begin
    let case_x = ((pos_x - x_init) / intervalle_x)
    and case_y = ((pos_y - y_init) / intervalle_y)
    in 
    let bmbs_adj = (nombre_bombes_adjacentes case_x case_y et_jeu) in
    let vide = ref false in
    let nouv_etat = ref
    (List.mapi (
      fun y ligne -> if y = case_y then (
        List.mapi (fun x valeur -> 
          if x = case_x then 
            (match valeur with
            | Cachee true -> Bombe
            | Cachee false -> 
              if bmbs_adj = 0 then begin
                vide := true;
                Vide
                end
              else Indice bmbs_adj
            | _ -> valeur) 
          else valeur)) ligne
    else ligne) et_jeu)
    and nouv_sc = ref sc
    in 
    if !vide then (* on révèle les cases adjacentes *)
      for i = -1 to 1 do
        for j = -1 to 1 do
          try
            match (List.nth (List.nth !nouv_etat (case_y+i)) (case_x+j)) with
            | Cachee false -> let n_etat, n_sc = (met_a_jour_clic !nouv_etat !nouv_sc (pos_x+(j*intervalle_x)) (pos_y+(i*intervalle_y)) x_init y_init intervalle_x intervalle_y) in
            nouv_etat := n_etat;
            nouv_sc := n_sc;
            | _ -> ()
          with
            | Invalid_argument _ -> ();
            | Failure _ -> ();
          done; done;
    if !nouv_etat = et_jeu then
      et_jeu, sc
    else
      match (List.nth (List.nth et_jeu case_y) case_x) with
      | Cachee true -> !nouv_etat, -1
      | _ -> !nouv_etat, (!nouv_sc+1)
  end;;

let affiche_fin gagne temps =
  clear_graph ();
  if gagne then begin
    let texte = (String.cat (String.cat "GAGNE ! Temps : " (string_of_int temps)) " secondes.") in
    let taille_x, taille_y = text_size texte in
    set_color black;
    moveto ((size_x () - taille_x) / 2) ((size_y () - taille_y) / 2);
    draw_string texte;
  end
  else begin
    let texte = "GAME OVER." in
    let taille_x, taille_y = text_size texte in
    set_color black;
    moveto ((size_x () - taille_x) / 2) ((size_y () - taille_y) / 2);
    draw_string texte;
  end;;

let rec attends_fermeture () =
  let e = wait_next_event [Button_down ; Key_pressed] in
  if not e.keypressed && e.button then attends_fermeture ();;

let () =
  let etat = ref EnCours
  and t = ref (int_of_float (Unix.gettimeofday ()))
  and t_init = (int_of_float (Unix.gettimeofday ()))
  and x = ref (size_x ())
  and y = ref (size_y ())
  and score = ref 0
  and etat_jeu = ref (jeu_initial (bombes_aleatoires ())) in
  charge_interface !score !etat_jeu t_init;
  while !etat = EnCours do
      let new_x = size_x ()
      and new_y = size_y ()
      and new_t = int_of_float (Unix.gettimeofday ()) in (* pour le moment *)
      if !x <> new_x || !y <> new_y || !t <> new_t then begin
        x := new_x;
        y := new_y;
        t := new_t;
        charge_interface !score !etat_jeu t_init;
      end;
      if !score < 0 then etat := Perdu
      else if !score >= 241 then etat := Gagne;
      if (button_down ()) then begin
        let x_i, y_i, i_x, i_y = coordonnees_quadrillage ()
        and pos_x, pos_y = (mouse_pos ()) in
        let et, s = (met_a_jour_clic !etat_jeu !score pos_x pos_y x_i y_i i_x i_y)
        in 
        etat_jeu := et;
        score := s;
        charge_interface !score !etat_jeu t_init;
      end;
  done;
  while !etat <> EnCours do
    match !etat with
    | Perdu -> affiche_fin false !t; attends_fermeture ();
    | Gagne -> affiche_fin true (!t-t_init); attends_fermeture ();
  done;;

