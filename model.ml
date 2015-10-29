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

(* $Id: model.ml,v 1.2 2003/11/13 16:08:44 simonet Exp $ *)

open Printf

exception Error of string



(***************************************************************************)
(** {2 Headers generators} *)

type generator =
    { remove: in_channel -> string;
      create: out_channel -> string list -> unit
    } 



(***************************************************************************)
(** {2 Models} *)

type model = (string * string) list -> generator

let models : (string, model) Hashtbl.t = 
  Hashtbl.create 3

let add name m =
  Hashtbl.add models name m

let find name =
  Hashtbl.find models name



(***************************************************************************)

let arg_string args ?default name =
  try
    List.assoc name args
  with
    Not_found ->
      match default with
	None -> raise (Error (sprintf "parameter %s is missing" name))
      |	Some s -> s

let arg_int args ?default name =
  try
    int_of_string (arg_string args ?default name)
  with
    Failure "int_of_string" -> 
      raise (Error (sprintf "parameter %s expects an integer" name))

let arg_char args ?default name =
  let s = arg_string args ?default name in
  if String.length s = 1 then s.[0]
  else raise (Error (sprintf "parameter %s expects a character" name))

    

let make_frame ~open_comment ~close_comment ~line_char ~margin ~width =

  let regexp_header = 
    Str.regexp_string (sprintf "%s%s" open_comment (String.make 10 line_char))
  in
  let regexp_blank = Str.regexp "^[ ]*$" in

  let remove ic =
    try
      let line = input_line ic in
      if Str.string_match regexp_header line 0
      then begin
	while not (Str.string_match regexp_blank (input_line ic) 0) do () done;
	""
      end
      else if Str.string_match regexp_blank line 0 
      then ""
      else (line ^ "\n")
    with End_of_file -> 
      ""
  in

  let create oc header =
    let width' = width + 2 * String.length margin in
    let white = String.make width' ' ' in
    let line = String.make width' line_char in
    Printf.fprintf oc "%s%s%s\n" open_comment line close_comment;
    
    List.iter (function string ->
      output_string oc open_comment;
      output_string oc margin;
      output_string oc string;
      output oc white 0 (max 0 (width - String.length string));
      output_string oc margin;
      output_string oc close_comment;
      output_char oc '\n'
    ) header;

    Printf.fprintf oc "%s%s%s\n\n" open_comment line close_comment
  in

  { remove = remove;
    create = create
  } 

let _ =
  add "frame" begin function args -> 
	make_frame 
	  ~open_comment:(arg_string args "open")
	  ~close_comment:(arg_string args "close")
	  ~line_char:(arg_char args "line")
	  ~margin:(arg_string args ~default:"  " "margin")
	  ~width:(arg_int args ~default:"68" "width")
  end



let make_lines ~open_comment ~close_comment ~line_char ~begin_line
    ~begin_last ~width =

  let regexp_begin = 
    Str.regexp_string (sprintf "%s%s" open_comment (String.make 10 line_char))
  in
  let regexp_end = 
    Str.regexp_string (sprintf "%s%s" (String.make 10 line_char) close_comment)
  in
  let end_length = 10 + String.length close_comment
  in

  let regexp_blank = Str.regexp "^[ ]*$" in

  let remove ic =
    try
      let line = input_line ic in
      if Str.string_match regexp_begin line 0
      then begin
	while
          let s = input_line ic in
            not (Str.string_match regexp_end s
                   (max 0 (String.length s - end_length)))
        do () done;
	""
      end
      else if Str.string_match regexp_blank line 0 
      then ""
      else (line ^ "\n")
    with End_of_file -> 
      ""
  in

  let create oc header =
    Printf.fprintf oc "%s%s\n" open_comment 
      (String.make (max 0 (width - String.length open_comment)) line_char);
    
    List.iter (function string ->
      output_string oc begin_line;
      output_string oc string;
      output_char oc '\n'
    ) header;

    Printf.fprintf oc "%s%s%s\n\n" 
      begin_last
      (String.make (max 0 (width - String.length begin_last
			     - String.length close_comment)) line_char)
      close_comment;

  in

  { remove = remove;
    create = create
  } 

let _ =
  add "lines" begin function args -> 
	make_lines 
	  ~open_comment:(arg_string args "open")
	  ~close_comment:(arg_string args "close")
	  ~line_char:(arg_char args "line")
	  ~begin_line:(arg_string args ~default:"  " "begin")
	  ~begin_last:(arg_string args ~default:"" "last")
	  ~width:(arg_int args ~default:"70" "width")
  end
      


let make_no () =

  { remove = (fun _ -> "");
    create = (fun _ _ -> ())
  } 

let _ =
  add "no" (function _ -> make_no ())