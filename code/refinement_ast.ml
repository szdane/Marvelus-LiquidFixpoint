(*
  A minimal, stand-alone AST + pretty-printer for Liquid-style
   refinement types of integers/booleans.

   usage:
   ocamlc -o pp_refinement refinement_ast.ml
   ./pp_refinement     
*)

type base_type =
  | TInt
  | TBool
  | TReal
  | TCustom of string        

type ident = string

type binop = Lt | Le | Gt | Ge | Eq | Neq | And | Or

type expr =
  | Var   of ident
  | Int   of int
  | Bool  of bool
  | Bin   of binop * expr * expr
  | Not   of expr

type refinement = { var : ident; base : base_type; pred : expr }
module Refinement_ast = struct 

  let string_of_base = function
    | TInt          -> "Int"
    | TBool         -> "Bool"
    | TReal         -> "Real"
    | TCustom name  -> name

  let string_of_binop = function
    | Lt  -> "<"  | Le  -> "<=" | Gt  -> ">"
    | Ge  -> ">=" | Eq  -> "="  | Neq -> "!="
    | And -> "&&" | Or  -> "||"

  let rec string_of_expr = function
    | Var  x               -> x
    | Int  n               -> string_of_int n
    | Bool b               -> string_of_bool b
    | Not e                -> "!(" ^ string_of_expr e ^ ")"
    | Bin (op, e1, e2)     ->
        string_of_expr e1 ^ " " ^ string_of_binop op ^ " " ^ string_of_expr e2 

  let string_of_refinement r =
    Printf.sprintf "{%s:%s | %s}"
      r.var
      (string_of_base r.base)
      (string_of_expr r.pred)
  end
(* 
let () =
  let r =
    { var  = "v"
    ; base = TInt
    ; pred = Bin (Lt, Var "v", Int 0)  (* v < 0 *)
    }
  in
  print_endline (Refinement_ast.string_of_refinement r) *)
  (* prints →  {v:Int | (v < 0)} *)
