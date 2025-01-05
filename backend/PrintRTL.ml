(* *********************************************************************)
(*                                                                     *)
(*              The Compcert verified compiler                         *)
(*                                                                     *)
(*          Xavier Leroy, INRIA Paris-Rocquencourt                     *)
(*                                                                     *)
(*  Copyright Institut National de Recherche en Informatique et en     *)
(*  Automatique.  All rights reserved.  This file is distributed       *)
(*  under the terms of the INRIA Non-Commercial License Agreement.     *)
(*                                                                     *)
(* *********************************************************************)

(** Pretty-printers for RTL code *)

open Printf
open Camlcoq
open Datatypes
open Maps
open AST
open RTL
open PrintAST

(* Printing of RTL code *)

let reg pp r =
  fprintf pp "x%d" (P.to_int r)

let rec regs pp = function
  | [] -> ()
  | [r] -> reg pp r
  | r1::rl -> fprintf pp "%a, %a" reg r1 regs rl

let ros pp = function
  | Coq_inl r -> reg pp r
  | Coq_inr s -> fprintf pp "\"%s\"" (extern_atom s)

let print_succ pp s dfl =
  let s = P.to_int s in
  if s <> dfl then fprintf pp "\tgoto %d\n" s

let print_xtype = function
  | Xbool -> "Xbool"
  | Xint8signed -> "Xint8signed"
  | Xint8unsigned -> "Xint8unsigned"
  | Xint16signed -> "Xint16signed"
  | Xint16unsigned -> "Xint16unsigned"
  | Xint -> "Xint"
  | Xfloat -> "Xfloat"
  | Xlong -> "Xlong"
  | Xsingle -> "Xsingle"
  | Xptr -> "Xptr"
  | Xany32 -> "Xany32"
  | Xany64 -> "Xany64"
  | Xvoid -> "Xvoid"

let print_sig p sg =
  List.iter
    (fun t -> fprintf p "%s,@ " (print_xtype t))
    sg.sig_args;
  fprintf p "%s" (print_xtype sg.sig_res)

let ident_name id = "\"" ^ Camlcoq.extern_atom id ^ "\""

let rec print_varlist p (vars, first) =
  match vars with
  | [] -> ()
  | v1 :: vl ->
      if not first then fprintf p ",@ ";
      fprintf p "%s" (ident_name v1);
      print_varlist p (vl, false)

let print_instruction pp (pc, i) =
  fprintf pp "%5d:\t" pc;
  match i with
  | Inop s ->
      let s = P.to_int s in
      fprintf pp "(%d, Inop %d)" pc s
  | Iop(op, args, res, s) ->
      let s = P.to_int s in
      fprintf pp "(%d, Iop %a %a %d)"
         pc reg res (PrintOp.print_operation reg) (op, args) s
  | Iload(chunk, addr, args, dst, s) ->
      let s = P.to_int s in
      fprintf pp "(%d, Iload %a %s %a %d)"
         pc reg dst (name_of_chunk chunk)
         (PrintOp.print_addressing reg) (addr, args) s
  | Istore(chunk, addr, args, src, s) ->
      let s = P.to_int s in
      fprintf pp "(%d, Istore %s %a %a %d)"
         pc (name_of_chunk chunk)
         (PrintOp.print_addressing reg) (addr, args)
         reg src s
  | Icall(sg, fn, args, res, s) ->
      fprintf pp "%a = %a(%a)\n"
        reg res ros fn regs args;
      print_succ pp s (pc - 1)
  | Itailcall(sg, fn, args) ->
      fprintf pp "tailcall %a(%a)\n"
        ros fn regs args
  | Ibuiltin(ef, args, res, s) ->
      fprintf pp "%a = %s(%a)\n"
        (print_builtin_res reg) res
        (name_of_external ef)
        (print_builtin_args reg) args;
      print_succ pp s (pc - 1)
  | Icond(cond, args, s1, s2) ->
      fprintf pp "if (%a) goto %d else goto %d\n"
        (PrintOp.print_condition reg) (cond, args)
        (P.to_int s1) (P.to_int s2)
  | Ijumptable(arg, tbl) ->
      let tbl = Array.of_list tbl in
      fprintf pp "jumptable (%a)\n" reg arg;
      for i = 0 to Array.length tbl - 1 do
        fprintf pp "\t\tcase %d: goto %d\n" i (P.to_int tbl.(i))
      done
  | Ireturn None ->
      fprintf pp "return\n"
  | Ireturn (Some arg) ->
      fprintf pp "return %a\n" reg arg

let print_code pp code =
  let instrs =
    List.sort
      (fun (pc1, _) (pc2, _) -> compare pc2 pc1)
      (List.rev_map
        (fun (pc, i) -> (P.to_int pc, i))
        (PTree.elements code)) in
  List.iter (print_instruction pp) instrs

let print_function pp id f =
  fprintf pp "(MkFunction @[<hov 4>%s @[<hov 0>(%a)@]@ @[<hov 0>(%a)@]@]"
    (extern_atom id)
    print_varlist (f.fn_params, true)
    print_sig f.fn_sig;
  fprintf pp "@[<v 2>@ ";
  fprintf pp "%ld;@ " (Z.to_int32 f.fn_stacksize);
  print_code pp f.fn_code;
  fprintf pp "%d;@ " (P.to_int f.fn_entrypoint);
  fprintf pp "@;<0 -2>@])@ "

let print_globdef pp (id, gd) =
  match gd with
  | Gfun(Internal f) -> print_function pp id f
  | _ -> ()

let print_program pp (prog: RTL.program) =
  List.iter (print_globdef pp) prog.prog_defs

let destination : string option ref = ref None

let print_if passno prog =
  match !destination with
  | None -> ()
  | Some f ->
      let oc = open_out (f ^ "." ^ Z.to_string passno) in
      print_program oc prog;
      close_out oc

