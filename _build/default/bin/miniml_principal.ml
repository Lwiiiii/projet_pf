(* ouverture de la "library" definie dans lib/dune *)
open Miniml

(* ouverture de modules de la library Miniml *)
open Miniml_lexer
open Miniml_parser
open Miniml_typer


(* ******** à compléter ********* *)
let () = 
  let flux = Miniml_parser.read_miniml_tokens_from_file "exemple.miniml" in
  let expr = Miniml_parser.parse_miniml flux in
  Miniml_parser.print_expr expr;
  let soltype = Miniml_typer.type_of expr in
  Miniml_typer.print_types soltype

