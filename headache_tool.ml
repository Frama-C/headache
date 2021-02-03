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
open Config_builtin
open Headache


(***************************************************************************)
(** {2 Configuration files} *)

let generators : (Str.regexp * Model.generator) list ref = ref []

let skips : (Str.regexp * Skip.param_skip) list ref = ref []

let read_configfile filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  try
    let (config_generators, config_skips) =
      (Config_parse.configfile Config_lex.token lexbuf)
    in
    skips := config_skips @ !skips;
    generators := config_generators @ !generators;
    close_in ic
  with
    Config.Error (msg, loc1, loc2) ->
      eprintf "%s: Configuration file %s, error at characters %d-%d:\n%s\n"
	Sys.argv.(0)
	filename loc1 loc2 msg;
      exit 2
  | Parsing.Parse_error ->
      eprintf "%s: Configuration file %s, syntax error at characters %d-%d:\n"
	Sys.argv.(0)
	filename (Lexing.lexeme_start lexbuf) (Lexing.lexeme_end lexbuf)

let find_generator filename =
  let basename = Filename.basename filename in
  try
    let _, generator =
      List.find (function regexp, _ ->
	Str.string_match regexp basename 0
      ) (! generators)
    in
    generator
  with
    Not_found ->
      eprintf "%s: No generator found for file %s\n"
	Sys.argv.(0) filename;
      exit 2

let find_skips filename =
  List.filter
    (fun (rg_filename, _) -> Str.string_match rg_filename filename 0)
    !skips



(***************************************************************************)
(** {2 Builtin config} *)

let _ =
  try
    generators :=
      List.map (function regexp, model, parameters ->
	(Str.regexp (sprintf "^%s$" regexp), (Model.find model) parameters)
      ) builtin_config
  with
    Not_found ->
      eprintf "%s: Error in builtin configuration\n" Sys.argv.(0);
      exit 2



(***************************************************************************)
(** {2 Header files} *)

let read_headerfile filename =
  let ic = open_in filename in
  let rec loop () =
    try
      let line = input_line ic in
      line :: loop ()
    with
      End_of_file -> close_in ic; []
  in
  let header =
    loop ()
  in
  let header_width =
    List.fold_left
      (fun w line -> max (Model.string_length line) w)
      0
      header
  in
    header, header_width

(***************************************************************************)
(** {2 Processing files} *)

let temp_file_counter = ref 0
let prng = Random.State.make_self_init ();;

let temp_file_name directory prefix suffix =
  let rnd = (Random.State.bits prng) land 0xFFFFFF in
  Filename.concat directory (Printf.sprintf "%s%06x%s" prefix rnd suffix)
;;

let open_temp_file ?(mode = [Open_text]) directory prefix suffix =
  let rec try_name counter =
    if counter >= 1000 then
      invalid_arg "Filename.open_temp_file: temp dir nonexistent or full";
    let name = temp_file_name directory prefix suffix in
    try
      (name,
       open_out_gen (Open_wronly::Open_creat::Open_excl::mode) 0o600 name)
    with Sys_error _ ->
      try_name (counter + 1)
  in try_name 0

let pipe_file f filename =
  let directory = Filename.dirname filename in
  let ic = open_in filename in
  let tempname, oc = open_temp_file directory "header" "tmp" in
  f ic oc;
  close_in ic;
  close_out oc;
  Unix.chmod tempname (Unix.stat filename).Unix.st_perm;
  Sys.remove filename;
  Sys.rename tempname filename

let rd_pipe_file f filename =
  let ic = open_in filename in
  f ic;
  close_in ic

let copy ic oc =
  let len = 256 in
  let buf = Bytes.create len in
  let rec loop () =
    let i = input ic buf 0 len in
    if i > 0 then begin
      output oc buf 0 i;
      loop ()
    end
  in
  loop ()

let create_header header header_width filename =
  let generator = find_generator filename in
  let skip_lst = find_skips filename in
  pipe_file (fun ic oc ->
    let () = Skip.skip skip_lst ic (Some oc) in
    let line = generator.Model.remove ic in
    let () = Skip.skip skip_lst ic (Some oc) in
    generator.Model.create oc header header_width;
    output_string oc line;
    copy ic oc
  ) filename



let remove_header filename =
  let generator = find_generator filename in
  let skip_lst = find_skips filename in
  pipe_file (fun ic oc ->
    let () = Skip.skip skip_lst ic (Some oc) in
    let line = generator.Model.remove ic in
    let () = Skip.skip skip_lst ic (Some oc) in
    output_string oc line;
    copy ic oc
  ) filename


let extract_header filename =
  let generator = find_generator filename in
  let skip_lst = find_skips filename in
  rd_pipe_file (fun ic ->
    let () = Skip.skip skip_lst ic None in
    generator.Model.extract ic
  ) filename





(***************************************************************************)
(** {2 Main loop} *)

type action =
    Create of string list * int
  | Remove
  | Extract


let main () =

  let action = ref (Create ([], 0)) in

  let anonymous filename =
    match !action with
      Create (header, header_width) -> create_header header header_width filename
    | Remove -> remove_header filename
    | Extract -> extract_header filename
  in

  Arg.parse [

  "-h",
  Arg.String (fun s -> let (header, header_width) = (read_headerfile s) in action := Create (header, header_width)),
  "<header-file>  Create headers with text from the header file";

  "-c",
  Arg.String read_configfile,
  "<config-file>  Read the given configuration file";

  "-r",
  Arg.Unit (fun () -> action := Remove),
  "               Remove headers in files";

  "-e",
  Arg.Unit (fun () -> action := Extract),
  "               Extract headers from files";

  ]

    anonymous

    (sprintf
       "%s, version %s\n\
       Usage: %s <options> <files>\n  \
       Process headers of given files\n\
       Optional arguments are:"
    Info.name Info.version Info.name);


  ()



let () =
  try
    main ()
  with
    Sys_error msg ->
      eprintf "%s: %s\n" Sys.argv.(0) msg
