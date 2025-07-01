(*
  Subtyping‑to‑Fixpoint generator (AST‑based version)

  Usage:
    ocamlfind ocamlopt -package str -linkpkg -o gen \
         refinement_ast.ml \
         gen_.ml
    ./gen_fq  or ./gen_fq mycheck.fq     
    fixpoint subtyping.fq or fixpoint mycheck.fq

*)
open Refinement_ast

module Ast_to_fq = struct
  (* One ‘bind’ line. *)
  let binding i (r : refinement) : string =
    Printf.sprintf "bind %d %s : %s" i r.var
      (Refinement_ast.string_of_refinement r)

  (* env [1;2;3; … n] *)
  let env_indices n =
    String.concat ";" (List.init n (fun k -> string_of_int (k + 1)))

  (* The constraint block that connects lhs ≤ rhs under the env. *)
  let constraint_block ~cid ~lhs ~rhs ~env_len =
    Printf.sprintf
      "constraint:\n  env [%s]\n  lhs %s\n  rhs %s\n  id %d tag []"
      (env_indices env_len)
      (Refinement_ast.string_of_refinement lhs)
      (Refinement_ast.string_of_refinement rhs)
      cid

  let to_fq ?(cid = 1) ~lhs ~rhs ~(env : refinement list) () : string =
    let binds = List.mapi (fun idx r -> binding (idx + 1) r) env in
    let cons  = constraint_block ~cid ~lhs ~rhs ~env_len:(List.length env) in
    String.concat "\n" (binds @ [cons]) ^ "\n"
end

let () =
  (* Example refinements *)
  let lhs =
    { var = "x";
      base = TInt;
      pred = Bin (Lt, Var "x", Int 0) }        (* {x:Int | x < 0} *)
  in
  let rhs =
    { var = "y";
      base = TInt;
      pred = Bin (Lt, Var "y", Var "u") }    (* {y:Int | y < u} *)
  in
  let env =
    [ { var = "u"; base = TInt;
        pred = Bin (Eq, Var "u", Int 5) };     (* u : {v:Int | v = 5} *)
      { var = "k"; base = TInt;
        pred = Bin (Eq, Var "k", Int 7) } ]    (* k : {v:Int | v = 7} *)
  in

  let fq_text = Ast_to_fq.to_fq ~cid:1 ~lhs ~rhs ~env () in

  (* Output filename: default ‘subtyping.fq’ or user‑supplied. *)
  let out_file =
    if Array.length Sys.argv > 1 then Sys.argv.(1) else "subtyping.fq"
  in
  let oc = open_out out_file in
  output_string oc fq_text;
  close_out oc;
  Printf.printf "Wrote Liquid‑Fixpoint fragment to %s\n%!" out_file
