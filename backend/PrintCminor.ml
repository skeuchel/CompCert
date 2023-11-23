(* *********************************************************************)
(*                                                                     *)
(*              The Compcert verified compiler                         *)
(*                                                                     *)
(*          Xavier Leroy, INRIA Paris-Rocquencourt                     *)
(*                                                                     *)
(*  Copyright Institut National de Recherche en Informatique et en     *)
(*  Automatique.  All rights reserved.  This file is distributed       *)
(*  under the terms of the GNU Lesser General Public License as        *)
(*  published by the Free Software Foundation, either version 2.1 of   *)
(*  the License, or  (at your option) any later version.               *)
(*  This file is also distributed under the terms of the               *)
(*  INRIA Non-Commercial License Agreement.                            *)
(*                                                                     *)
(* *********************************************************************)

(** Pretty-printer for Cminor *)

open Format
open! Camlcoq
open Integers
open AST
open PrintAST
open Cminor

(* Naming idents. *)

let ident_name id = Camlcoq.extern_atom id

(* Naming operators *)

let name_of_unop = function
  | Ocast8unsigned  -> "Ocast8unsigned"
  | Ocast8signed    -> "Ocast8signed"
  | Ocast16unsigned -> "Ocast16unsigned"
  | Ocast16signed   -> "Ocast16signed"
  | Onegint         -> "Onegint"
  | Onotint         -> "Onotint"
  | Onegf           -> "Onegf"
  | Oabsf           -> "Oabsf"
  | Onegfs          -> "Onegfs"
  | Oabsfs          -> "Oabsfs"
  | Osingleoffloat  -> "Osingleoffloat"
  | Ofloatofsingle  -> "Ofloatofsingle"
  | Ointoffloat     -> "Ointoffloat"
  | Ointuoffloat    -> "Ointuoffloat"
  | Ofloatofint     -> "Ofloatofint"
  | Ofloatofintu    -> "Ofloatofintu"
  | Ointofsingle    -> "Ointofsingle"
  | Ointuofsingle   -> "Ointuofsingle"
  | Osingleofint    -> "Osingleofint"
  | Osingleofintu   -> "Osingleofintu"
  | Onegl           -> "Onegl"
  | Onotl           -> "Onotl"
  | Ointoflong      -> "Ointoflong"
  | Olongofint      -> "Olongofint"
  | Olongofintu     -> "Olongofintu"
  | Olongoffloat    -> "Olongoffloat"
  | Olonguoffloat   -> "Olonguoffloat"
  | Ofloatoflong    -> "Ofloatoflong"
  | Ofloatoflongu   -> "Ofloatoflongu"
  | Olongofsingle   -> "Olongofsingle"
  | Olonguofsingle  -> "Olonguofsingle"
  | Osingleoflong   -> "Osingleoflong"
  | Osingleoflongu  -> "Osingleoflongu"

let comparison_name = function
  | Ceq -> "Ceq"
  | Cne -> "Cne"
  | Clt -> "Clt"
  | Cle -> "Cle"
  | Cgt -> "Cgt"
  | Cge -> "Cge"

let name_of_binop = function
  | Oadd     -> "Oadd"
  | Osub     -> "Osub"
  | Omul     -> "Omul"
  | Odiv     -> "Odiv"
  | Odivu    -> "Odivu"
  | Omod     -> "Omod"
  | Omodu    -> "Omodu"
  | Oand     -> "Oand"
  | Oor      -> "Oor"
  | Oxor     -> "Oxor"
  | Oshl     -> "Oshl"
  | Oshr     -> "Oshr"
  | Oshru    -> "Oshru"
  | Oaddf    -> "Oaddf"
  | Osubf    -> "Osubf"
  | Omulf    -> "Omulf"
  | Odivf    -> "Odivf"
  | Oaddfs   -> "Oaddfs"
  | Osubfs   -> "Osubfs"
  | Omulfs   -> "Omulfs"
  | Odivfs   -> "Odivfs"
  | Oaddl    -> "Oaddl"
  | Osubl    -> "Osubl"
  | Omull    -> "Omull"
  | Odivl    -> "Odivl"
  | Odivlu   -> "Odivlu"
  | Omodl    -> "Omodl"
  | Omodlu   -> "Omodlu"
  | Oandl    -> "Oandl"
  | Oorl     -> "Oorl"
  | Oxorl    -> "Oxorl"
  | Oshll    -> "Oshll"
  | Oshrl    -> "Oshrl"
  | Oshrlu   -> "Oshrlu"
  | Ocmp c   -> "(Ocmp " ^ comparison_name c ^")"
  | Ocmpu c  -> "(Ocmpu " ^ comparison_name c ^")"
  | Ocmpf c  -> "(Ocmpf " ^ comparison_name c ^")"
  | Ocmpfs c -> "(Ocmpfs " ^ comparison_name c ^")"
  | Ocmpl c  -> "(Ocmpl " ^ comparison_name c ^")"
  | Ocmplu c -> "(Ocmplu " ^ comparison_name c ^")"

(* Expressions *)

let rec expr p e =
  fprintf p "@[<hov 2>";
  begin match e with
  | Evar id ->
      fprintf p "(Evar %s)" (ident_name id)
  | Econst(Ointconst n) ->
      fprintf p "(Econst (Ointconst %ld))" (camlint_of_coqint n)
  | Econst(Ofloatconst f) ->
      fprintf p "(Econst (Ofloatconst %.15F))" (camlfloat_of_coqfloat f)
  | Econst(Osingleconst f) ->
      fprintf p "(Econst (Osingleconst %.15Ff))" (camlfloat_of_coqfloat32 f)
  | Econst(Olongconst n) ->
      fprintf p "(Econst (Olongconst %LdLL))" (camlint64_of_coqint n)
  | Econst(Oaddrsymbol(id, ofs)) ->
      let ofs = camlint_of_coqint ofs in
      fprintf p "(Econst (Oaddrsymbol %s %ld))" (extern_atom id) ofs
  | Econst(Oaddrstack n) ->
      fprintf p "(Econst (Oaddrstack %ld))" (camlint_of_coqint n)
  | Eunop(op, a1) ->
      fprintf p "(Eunop %s %a)" (name_of_unop op) expr a1
  | Ebinop(op, a1, a2) ->
      fprintf p "(Ebinop %s@ %a@ %a)"
        (name_of_binop op) expr a1 expr a2
  | Eload(chunk, a1) ->
      fprintf p "(Eload %s %a)" (name_of_chunk chunk) expr a1
  end;
  fprintf p "@]"

let print_expr p e = expr p e

let rec print_expr_list p (first, rl) =
  match rl with
  | [] -> ()
  | r :: rl ->
      if not first then fprintf p ",@ ";
      expr p r;
      print_expr_list p (false, rl)

(* Types *)

let name_of_type = function
  | Tint -> "Tint"
  | Tfloat -> "Tfloat"
  | Tlong -> "Tlong"
  | Tsingle -> "Tsingle"
  | Tany32 -> "Tany32"
  | Tany64 -> "Tany64"

let rec print_type_list p (first, tl) =
  match tl with
  | [] -> ()
  | t :: tl ->
     if not first then fprintf p "@ ";
     fprintf p "%s" (name_of_type t);
     print_type_list p (false, tl)

let print_sig p sg =
  fprintf p "@[<hov 0>(signature@ (%a)@ %s)@]"
    print_type_list (true, sg.sig_args)
    (name_of_rettype sg.sig_res)

let rec just_skips s =
  match s with
  | Sskip -> true
  | Sseq(s1,s2) -> just_skips s1 && just_skips s2
  | _ -> false


(* Statements *)

let rec print_stmt p s =
  match s with
  | Sskip ->
      fprintf p "(Sskip)"
  | Sassign(id, e2) ->
      fprintf p "@[<hv 2>(Sassign %s %a)@]" (ident_name id) print_expr e2
  | Sstore(chunk, a1, a2) ->
      fprintf p "@[<hv 2>(Sstore %s %a@ %a)@]"
              (name_of_chunk chunk) print_expr a1 print_expr a2
  | Scall(None, sg, e1, el) ->
      fprintf p "@[<hv 2>%a@,(@[<hov 0>%a@])@ : @[<hov 0>%a@];@]"
                print_expr e1
                print_expr_list (true, el)
                print_sig sg
  | Scall(Some id, sg, e1, el) ->
      fprintf p "@[<hv 2>%s =@ %a@,(@[<hov 0>%a@])@] : @[<hov 0>%a;@]"
                (ident_name id)
                print_expr e1
                print_expr_list (true, el)
                print_sig sg
  | Stailcall(sg, e1, el) ->
      fprintf p "@[<hv 2>tailcall %a@,(@[<hov 0>%a@])@ : @[<hov 0>%a@];@]"
                print_expr e1
                print_expr_list (true, el)
                print_sig sg
  | Sbuiltin(None, ef, el) ->
      fprintf p "@[<hv 2>builtin %s@,(@[<hov 0>%a@])@ : @[<hov 0>%a@];@]"
                (name_of_external ef)
                print_expr_list (true, el)
	        print_sig (ef_sig ef)
  | Sbuiltin(Some id, ef, el) ->
      fprintf p "@[<hv 2>%s =@ builtin %s@,(@[<hov 0>%a@]) : @[<hov 0>%a@];@]"
                (ident_name id)
                (name_of_external ef)
                print_expr_list (true, el)
	        print_sig (ef_sig ef)
  | Sseq(s1,s2) when just_skips s1 && just_skips s2 ->
      fprintf p "(Sskip)"
  | Sseq(s1, s2) when just_skips s1 ->
      print_stmt p s2
  | Sseq(s1, s2) when just_skips s2 ->
      print_stmt p s1
  | Sseq(s1, s2) ->
      fprintf p "@[<v 2>(Sseq %a@ %a)@]" print_stmt s1 print_stmt s2
(*  | Sifthenelse(e, s1, Sskip) ->
      fprintf p "@[<v 2>if (%a) {@ %a@;<0 -2>}@]"
              print_expr e
              print_stmt s1 *)
(*  | Sifthenelse(e, Sskip, s2) ->
      fprintf p "@[<v 2>if (! %a) {@ %a@;<0 -2>}@]"
              expr (15, e)
              print_stmt s2 *)
  | Sifthenelse(e, s1, s2) ->
      fprintf p "@[<v 2>(Sifthenelse %a@ %a@ %a)@]"
              print_expr e
              print_stmt s1
              print_stmt s2
  | Sloop(s) ->
      fprintf p "@[<v 2>(Sloop@ %a)@]"
              print_stmt s
  | Sblock(s) ->
      fprintf p "@[<v 2>(Sblock@ %a)@]"
              print_stmt s
  | Sexit n ->
      fprintf p "(Sexit %d)" (Nat.to_int n)
  | Sswitch(long, e, cases, dfl) ->
    let print_case (n,x) =
      let x = Nat.to_int x in
      if long then
        fprintf p "@ case %LdLL: exit %d;" (Z.to_int64 n) x
      else
        fprintf p "@ case %ld: exit %d;" (Z.to_int32 n) x in
      fprintf p "@[<v 2>switch%s (%a) {"
        (if long then "l" else "") print_expr e;
      List.iter print_case cases;
      fprintf p "@ default: exit %d;\n" (Nat.to_int dfl);
      fprintf p "@;<0 -2>}@]"
  | Sreturn None ->
      fprintf p "(Sreturn)"
  | Sreturn (Some e) ->
      fprintf p "(Sreturn %a)" print_expr e
  | Slabel(lbl, s1) ->
      fprintf p "(Slabel %s@ %a)" (ident_name lbl) print_stmt s1  (* wrong for Cminorgen output *)
  | Sgoto lbl ->
      fprintf p "(Sgoto %s)" (ident_name lbl)               (* wrong for Cminorgen output *)

(* Functions *)

let rec print_varlist p (vars, first) =
  match vars with
  | [] -> ()
  | v1 :: vl ->
      if not first then fprintf p "@ ";
      fprintf p "%s" (ident_name v1);
      print_varlist p (vl, false)

let print_function p id f =
  fprintf p "@[<hov 2>(procedure %s@ (@[<hov 0>%a@])@ @[<hov 0>%a@]@ "
            (extern_atom id)
            print_varlist (f.fn_params, true)
            print_sig f.fn_sig;
  let stksz = Z.to_int32 f.fn_stackspace in
  fprintf p "(stack %ld)@ " stksz;
  fprintf p "(vars@[<hov 0>%a)@]@ " print_varlist (f.fn_vars, false);
  print_stmt p f.fn_body; 
  fprintf p ")@]@ "

let print_extfun p id ef =
  fprintf p "@[<v 2>(extern@ %s@ (%s)@ %a)@]@ "
    (extern_atom id) (name_of_external ef) print_sig (ef_sig ef)

let print_init_data p = function
  | Init_int8 i -> fprintf p "int8 %ld" (camlint_of_coqint i)
  | Init_int16 i -> fprintf p "int16 %ld" (camlint_of_coqint i)
  | Init_int32 i -> fprintf p "%ld" (camlint_of_coqint i)
  | Init_int64 i -> fprintf p "%LdLL" (camlint64_of_coqint i)
  | Init_float32 f -> fprintf p "float32 %.15F" (camlfloat_of_coqfloat f)
  | Init_float64 f -> fprintf p "%.15F" (camlfloat_of_coqfloat f)
  | Init_space i -> fprintf p "[%s]" (Z.to_string i)
  | Init_addrof(id,off) -> fprintf p "%ld(\"%s\")" (camlint_of_coqint off) (extern_atom id)

let rec print_init_data_list p = function
  | [] -> ()
  | [item] -> print_init_data p item
  | item::rest ->
      (print_init_data p item;
       fprintf p ",";
       print_init_data_list p rest)

let print_globvar p gv =
  if (gv.gvar_readonly) then
    fprintf p "readonly ";
  if (gv.gvar_volatile) then
    fprintf p "volatile ";
  fprintf p "{";
  print_init_data_list p gv.gvar_init;
  fprintf p "}"

let print_globdef p (id, gd) =
  match gd with
  | Gfun(External ef) ->
      () (* print_extfun p id ef *)
  | Gfun(Internal f) ->
      print_function p id f
  | Gvar gv ->
     fprintf p "(globalvar %s %a)\n" (extern_atom id) print_globvar gv

let print_program p prog =
  fprintf p "@[<hov 2>(program@ ";
  if extern_atom prog.prog_main <> "main" then
    fprintf p "= \"%s\"\n" (extern_atom prog.prog_main);
  List.iter (print_globdef p) prog.prog_defs;
  fprintf p ")@]@."

let destination : string option ref = ref None

let print_if prog =
  match !destination with
  | None -> ()
  | Some f ->
      let oc = open_out f in
      print_program (formatter_of_out_channel oc) prog;
      close_out oc


