/**************************************************************************/
/*                                                                        */
/*                               Headache                                 */
/*                                                                        */
/*          Vincent Simonet, Projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*  Copyright 2002                                                        */
/*  Institut National de Recherche en Informatique et en Automatique.     */
/*  All rights reserved.  This file is distributed under the terms of     */
/*  the GNU Library General Public License.                               */
/*                                                                        */
/*  Vincent.Simonet@inria.fr           http://cristal.inria.fr/~simonet/  */
/*                                                                        */
/**************************************************************************/

%{
open Printf

type entry = 
  | EntryModel of Model.generator 
  | EntrySkip of Skip.param_skip list
;;

(* Dispatch entry considering if it is a skip or a model.
 * List returned are reversed considering their initial order.
 *)
let rec dispatch_entry acc_model acc_skip lst =
  match lst with
  | (rg_filename, EntryModel mdl) :: tl ->
      dispatch_entry ((rg_filename, mdl) :: acc_model) acc_skip tl
  | (rg_filename, EntrySkip rg_skip_lst) :: tl ->
      let nacc_skip =
        List.fold_left 
        (fun nacc_skip rg_skip -> (rg_filename, rg_skip) :: nacc_skip) 
        acc_skip
        rg_skip_lst
      in
      dispatch_entry acc_model nacc_skip tl
  | [] ->
      acc_model, acc_skip
%}

%token ARROW
%token COLON
%token EOF
%token PIPE
%token <string> STRING

%start configfile
%type <((Str.regexp * Model.generator) list) * ((Str.regexp * Skip.param_skip) list)> configfile
%start boot
%type <(string * string * (string * string) list) list> boot

%%

configfile:
  opt_pipe item_list EOF                            { dispatch_entry [] [] $2 }
;

opt_pipe:
  /*empty*/                                         { () }
| PIPE                                              { () }
;

item_list:
  item                                              { $1 :: [] }
| item_list PIPE item                               { $3 :: $1 }
;

item:
  STRING ARROW STRING parameters                    
  { 
    let regexp =
      try 
	Str.regexp ("^" ^ $1 ^ "$")
      with
	Failure msg ->
	  raise (Config.Error (sprintf "Illegal regexp: %s" msg,
			       Parsing.rhs_start 1, Parsing.rhs_end 1))
    in
    if $3 = "skip" then
      let fun_parameters (id, str) =
        if id = "match" then
          try 
            false,Str.regexp ("^" ^ str ^ "$")
          with
            Failure msg ->
              raise (Config.Error (sprintf "Illegal regexp: %s" msg,
                                   Parsing.rhs_start 1, Parsing.rhs_end 1))
        else if id = "multiline_match" then
          try
            true,(Str.regexp ("^" ^ str ^ "$"))
          with
            Failure msg ->
              raise (Config.Error (sprintf "Illegal regexp: %s" msg,
                                   Parsing.rhs_start 1, Parsing.rhs_end 1))
        else
          raise (Config.Error (sprintf "Unkown option '%s' for skip" id,
                                 Parsing.rhs_start 3, Parsing.rhs_end 3))
      in
      let skip_lst =
        List.map fun_parameters (List.rev $4)
      in
      regexp, (EntrySkip skip_lst)
    else
      let model =
        try
          Model.find $3
        with
          Not_found ->
            raise (Config.Error (sprintf "Unknown model: %s" $3,
                                 Parsing.rhs_start 3, Parsing.rhs_end 3))
      in
      let generator =
        try
          model (List.rev $4)
        with
          Model.Error msg ->
            raise (Config.Error (msg,
                                 Parsing.rhs_start 3, Parsing.rhs_end 4))
      in
      regexp, (EntryModel generator)
  }
;

parameters:
  /*empty*/                                         { [] }
| parameters STRING COLON STRING                    { ($2, $4) :: $1 }
;



/***************************************************************************/

boot:
  opt_pipe boot_item_list EOF                       { List.rev $2 }
;


boot_item_list:
  boot_item                                         { $1 :: [] }
| boot_item_list PIPE boot_item                     { $3 :: $1 }
;

boot_item:
  STRING ARROW STRING parameters                    { $1, $3, $4 }
;
