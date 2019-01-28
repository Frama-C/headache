(**************************************************************************)
(*                                                                        *)
(*                               Headache                                 *)
(*                                                                        *)
(*          Vincent Simonet, Projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*  Copyright 2002                                                        *)
(*  Institut National de Recherche en Informatique et en Automatique.     *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Library General Public License.                               *)
(*                                                                        *)
(*  Vincent.Simonet@inria.fr           http://cristal.inria.fr/~simonet/  *)
(*                                                                        *)
(**************************************************************************)

open Printf
open Headache

let file_in = "config_builtin.txt"
let file_out = "config_builtin.ml"

let main () =
  let ic = open_in file_in in
  let lexbuf = Lexing.from_channel ic in
  let oc = open_out file_out in
  try
    fprintf oc "let builtin_config = [\n";
    List.iter (function regexp, model, parameters ->
      fprintf oc "\"%s\", \"%s\", [" 
	(String.escaped regexp) (String.escaped model);
      List.iter (function name, v ->
	fprintf oc "\"%s\", \"%s\"; " (String.escaped name) (String.escaped v)
      ) parameters;
      fprintf oc "];\n"
    ) (Config_parse.boot Config_lex.token lexbuf);
    fprintf oc "]\n";
    close_in ic;
    close_out oc
  with
    Config.Error (msg, loc1, loc2) ->
      eprintf "%s: Configuration file %s, error at characters %d-%d:\n%s\n"
	Sys.argv.(0)
	file_in loc1 loc2 msg;
      exit 2
  | Parsing.Parse_error ->
      eprintf "%s: Configuration file %s, syntax error at characters %d-%d:\n"
	Sys.argv.(0)
	file_in (Lexing.lexeme_start lexbuf) (Lexing.lexeme_end lexbuf)


let () = 
  main ()
