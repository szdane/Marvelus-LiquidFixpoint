(*---------------------------------------------------------------------------
  Subtyping‑to‑Fixpoint generator
  --------------------------------
  Usage:
      let fq = Refined_to_fq.refined_pair_to_fq
                  ~cid:1 ~bid:1
                  "{x:Int | x<5}"
                  "{y:Int | y < 0}" in
      print_endline fq
  ---------------------------------------------------------------------------*)

  module Refined_to_fq = struct
    (* ───────── helpers ─────────                                                     *)
  
    (* Regex to capture  { var : sort | predicate } *)
    let re_refined =
      Str.regexp
        "\\{[ \t\n]*\\([A-Za-z_][A-Za-z0-9_]*\\)[ \t\n]*:[ \t\n]*\
         \\([A-Za-z][A-Za-z0-9_]*\\)[ \t\n]*|[ \t\n]*\\([^}]+\\)[ \t\n]*\\}"
  
    (* Parse one refined‑type literal *)
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
  
    (* ───────── public API ─────────                                                 *)
  
    let refined_pair_to_fq ?(cid = 1) ?(bid = 1) t1 t2 =
      let v1, sort1, p1 = parse_refined t1 in
      let v2, sort2, p2_raw = parse_refined t2 in
      if sort1 <> sort2 then
        failwith ("Sort mismatch: " ^ sort1 ^ " vs " ^ sort2);
  
      let p2 = alpha_convert ~old_var:v2 ~new_var:v1 p2_raw in
  
      Printf.sprintf
        "constraint:\n\
         \  env []\n\
         \  lhs {%s:%s | %s}\n\
         \  rhs {%s:%s | %s}\n\
         \  id  %d tag []"
        v1 sort1 p1 v1 sort1 p2 cid
  end
  
  (* ---------------- example CLI ---------------- *)
  let () =
  (* Example refined types; replace or make them CLI arguments if you like *)
  let t_left  = "{x:Int | x<0}"
  and t_right = "{y:Int | y<5}" in

  let fq_text =
    Refined_to_fq.refined_pair_to_fq ~cid:1 ~bid:1 t_left t_right in

  (* Output filename: first CLI arg or default "subtyping.fq" *)
  let out_file =
    if Array.length Sys.argv > 1 then Sys.argv.(1) else "subtyping.fq" in

  let oc = open_out out_file in
  output_string oc fq_text;
  close_out oc;

  Printf.printf "Wrote Liquid‑Fixpoint fragment to %s\n" out_file