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

/**************************************************************************/
/*                                                                        */
/*                                 Header                                 */
/*                 Automatic generation of files headers                  */
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

/**************************************************************************/
/*                                                                        */
/*                                 Header                                 */
/*                 Automatic generation of files headers                  */
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

/* $Id: config_parse.mly,v 1.2 2003/11/13 16:08:44 simonet Exp $ */

%{
open Printf
%}

%token ARROW
%token COLON
%token EOF
%token PIPE
%token <string> STRING

%start configfile
%type <(Str.regexp * Model.generator) list > configfile
%start boot
%type <(string * string * (string * string) list) list> boot

%%

configfile:
  opt_pipe item_list EOF                            { List.rev $2 }
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
    regexp, generator
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
