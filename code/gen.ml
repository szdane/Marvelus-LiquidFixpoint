(*---------------------------------------------------------------------------
  Subtyping‑to‑Fixpoint generator
  --------------------------------
  Usage:
    ocamlfind ocamlopt -package str -linkpkg gen.ml -o gen_fq                  
  ./gen_fq mycheck.fq
  ---------------------------------------------------------------------------*)

  module Refined_to_fq = struct
    let re_refined =
      Str.regexp
        "\\{[ \t\n]*\\([A-Za-z_][A-Za-z0-9_]*\\)[ \t\n]*:[ \t\n]*\
         \\([A-Za-z][A-Za-z0-9_]*\\)[ \t\n]*|[ \t\n]*\\([^}]+\\)[ \t\n]*\\}"
  
    let parse_refined rt =
      if Str.string_match re_refined rt 0 then
        let var  = Str.matched_group 1 rt in
        let sort = Str.matched_group 2 rt in
        let pred = Str.matched_group 3 rt in
        (var, sort, pred)
      else
        failwith ("Bad refinement type: " ^ rt)
  
    (* Replace all occurrences of old_var with new_var in pred (plain string replace) *)
    let alpha_convert ~old_var ~new_var pred =
      if old_var = new_var then pred
      else
        Str.global_replace (Str.regexp_string old_var) new_var pred
    
    let bind_printer i env =  "bind " ^ i ^ " " ^ env   
    
    let rec all_bindings n i env = 
      match env with 
        | [] -> "" 
        | x::xs -> (bind_printer (string_of_int i) x) ^ "\n" ^ (all_bindings n (i+1) xs)
    
    let env_elem n = string_of_int n ^ ";"

    let rec env_list n = match n with
      |0->""
      |1->string_of_int n
      |_ -> env_elem n ^ " " ^ env_list (n-1)
  
    let refined_pair_to_fq ?(cid = 1) ?(bid = 1) t1 t2 env =
      let v1, sort1, p1 = parse_refined t1 in
      let v2, sort2, p2_raw = parse_refined t2 in
      if sort1 <> sort2 then
        failwith ("Sort mismatch: " ^ sort1 ^ " vs " ^ sort2);
      
      let p2 = alpha_convert ~old_var:v2 ~new_var:v1 p2_raw 
      and binds = all_bindings (List.length env) 1 env
      and env_vars = env_list (List.length env) in
      Printf.sprintf
        "%s \n\
        constraint:\n\
         \  env [%s]\n\
         \  lhs {%s:%s | %s}\n\
         \  rhs {%s:%s | %s}\n\
         \  id  %d tag []"
      binds env_vars v1 sort1 p1 v1 sort1 p2 cid
  end
  
  let () =
  let t_left  = "{x:Int | x<k}"
  and t_right = "{y:Int | y<u}" 
  and env = "u: {v: int | v = 5}"::"k: {v: Int | v=7}"::[] in

  let fq_text =
    Refined_to_fq.refined_pair_to_fq ~cid:1 ~bid:1 t_left t_right env in

  (* Output filename: first CLI arg or default "subtyping.fq" *)
  let out_file =
    if Array.length Sys.argv > 1 then Sys.argv.(1) else "subtyping.fq" in

  let oc = open_out out_file in
  output_string oc fq_text;
  close_out oc;

  Printf.printf "Wrote Liquid‑Fixpoint fragment to %s\n" out_file