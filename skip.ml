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


type regexp_filename = Str.regexp
;;

type regexp_skip = Str.regexp
;;

let skip skip_lst ic oc =
  let skip_aux () =
    let initial_pos =
      LargeFile.pos_in ic
    in
    try
      let line =
        input_line ic
      in
      try
        let _ =
          List.find
            (fun (_, rg_skip) -> Str.string_match rg_skip line 0)
            skip_lst
        in
          prerr_endline
            ("Line : "^line^" skipped");
          match oc with
          | None -> ()
          | Some oc ->
            output_string oc line;
            output_string oc "\n"
      with Not_found ->
        LargeFile.seek_in ic initial_pos
    with End_of_file ->
      ()
  in
    skip_aux ()
;;
