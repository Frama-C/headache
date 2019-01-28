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

{
open Printf
open Config_parse



(* To buffer string literals *)

let initial_string_buffer = Bytes.create 256
let string_buff = ref initial_string_buffer
let string_index = ref 0

let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0

let store_string_char c =
  let len = Bytes.length (!string_buff) in
  if !string_index >= len then begin
    let new_buff = Bytes.create (len * 2) in
      Bytes.blit (!string_buff) 0 new_buff 0 len;
      string_buff := new_buff
  end;
  Bytes.set (!string_buff) (!string_index) c;
  incr string_index

let get_stored_string () =
  let s = Bytes.sub (!string_buff) 0 (!string_index) in
  string_buff := initial_string_buffer;
  s

(* To translate escape sequences *)

let char_for_backslash =
  match Sys.os_type with
  | "Unix" | "Win32" | "Cygwin" ->
      begin function
      | 'n' -> '\010'
      | 'r' -> '\013'
      | 'b' -> '\008'
      | 't' -> '\009'
      | c   -> c
      end
  | "MacOS" ->
      begin function
      | 'n' -> '\013'
      | 'r' -> '\010'
      | 'b' -> '\008'
      | 't' -> '\009'
      | c   -> c
      end
  | _ -> (* TEMPORARY Error "Unknown system type" *) assert false

let char_for_decimal_code lexbuf i =
  let c = 100 * (Char.code(Lexing.lexeme_char lexbuf i) - 48) +
           10 * (Char.code(Lexing.lexeme_char lexbuf (i+1)) - 48) +
                (Char.code(Lexing.lexeme_char lexbuf (i+2)) - 48) in  
  if c < 0 || c > 255 then 
    raise (Config.Error(sprintf "illegal escape %s" (Lexing.lexeme lexbuf),
		 Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf))
  else Char.chr c

(* To store the position of the beginning of a string *)
let string_start_pos = ref 0

} 

let blank = [' ' '\010' '\013' '\009' '\012']
let identchar = 
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']



rule token = parse
    blank +
      { token lexbuf }
  | "->"
      { ARROW }
  | "|"
      { PIPE }
  | ":"
      { COLON }
  | "#" [^ '\010' '\013']* ['\010' '\013']
      { token lexbuf }
  | identchar+
      { STRING (Lexing.lexeme lexbuf) }
  | "\""
      { reset_string_buffer();
        let string_start = Lexing.lexeme_start lexbuf in
        string_start_pos := string_start;
        string lexbuf;
        lexbuf.Lexing.lex_start_pos <-
          string_start - lexbuf.Lexing.lex_abs_pos;
        STRING (Bytes.to_string (get_stored_string())) }
  | eof
      { EOF }
  | _
      { raise (Config.Error (sprintf "Illegal character %c"
			((Lexing.lexeme lexbuf).[0]),
		      Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf))
      }	



and string = parse
    '"'
      { () }
  | '\\' ("\010" | "\013" | "\013\010") [' ' '\009'] *
      { string lexbuf }
  | '\\' ['\\' '"' 'n' 't' 'b' 'r']
      { store_string_char(char_for_backslash(Lexing.lexeme_char lexbuf 1));
        string lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
      { store_string_char(char_for_decimal_code lexbuf 1);
         string lexbuf }
  | eof
      { raise (Config.Error ("Unterminated string",
		      !string_start_pos, !string_start_pos+1))
      }
  | _
      { store_string_char(Lexing.lexeme_char lexbuf 0);
        string lexbuf }
