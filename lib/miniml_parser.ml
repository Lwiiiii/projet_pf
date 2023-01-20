open Miniml_types
open Miniml_lexer
open Lazyflux
open Miniml_parsertype
open Parser

(* Fonction de lecture d'un fichier.    *)
(* Produit le flux des lexèmes reconnus *)
let read_miniml_tokens_from_file filename : token Flux.t =
  try
    let chan = open_in filename in
    let buf = Lexing.from_channel chan in
    line_g := 1;
    let next_token () =
      try
        let next = token buf in
        if next = EOF
        then
          begin
            close_in chan;
            None
          end
        else
          Some (next, ())
   with
   | ErreurLex msg ->
      begin
        close_in chan;
        raise (ErreurLecture (Format.sprintf "ERREUR : ligne %d, lexème '%s' : %s" !line_g (Lexing.lexeme buf) msg))
      end in
    Flux.unfold next_token ()
 with
    | Sys_error _ -> raise (ErreurLecture (Format.sprintf "ERREUR : Impossible d'ouvrir le fichier '%s' !" filename))
;;

(* Fonction de lecture d'un buffer.   *)
(* Similaire à la fonction précédente *)
let read_miniml_tokens_from_lexbuf buf : token Flux.t =
  line_g := 1;
  let next_token () =
    try
      let next = token buf in
      if next = EOF
      then
        begin
          None
        end
      else
        Some (next, ())
    with
    | ErreurLex msg ->
       begin
         raise (ErreurLecture (Format.sprintf "ERREUR : ligne %d, lexème '%s' : %s" !line_g (Lexing.lexeme buf) msg))
       end in
  Flux.unfold next_token ()
;;

(* Fonction de lecture d'une chaîne.  *)
(* Similaire à la fonction précédente *)
let read_miniml_tokens_from_string chaine : token Flux.t =
  read_miniml_tokens_from_lexbuf (Lexing.from_string chaine)
;;

(* Fonctions auxiliaires de traitement des lexèmes *)
(* contenant une information: IDENT, BOOL et INT   *)
let isident =
  function IDENT _     -> true
         | _           -> false
let isbool =
  function BOOL _      -> true
         | _           -> false
let isint =
  function INT _       -> true
         | _           -> false

let unident =
  function
  | IDENT id    -> id
  | _           -> assert false
let unbool =
  function
  | BOOL b      -> b
  | _           -> assert false   
let unint =
  function
  | INT i       -> i
  | _           -> assert false

(* Fonctions de parsing de MiniML *)
(* ******** à compléter ********* *)




let rec parse_Expr flux =
  (
  (
    p_token LET *> p_ident *> p_token EQU *> parse_Expr *> p_token IN *> parse_Expr >>= fun(((((_, ident), _), expr1), _), expr2) -> return (ELet((unident ident), expr1, expr2))
  ) ++ (
    p_token LET *> p_token REC *> p_ident *> p_token EQU *> parse_Expr *> p_token IN *> parse_Expr >>= fun((((((_,_), ident), _), expr1), _), expr2) -> return (ELetrec(unident ident, expr1, expr2))
  ) ++ (
    p_token PARO *> parse_Expr *> parse_Binop *> parse_Expr *> p_token PARF >>= fun((((_, expr1), binop), expr2), _) -> return (EApply(EApply(binop, expr1), expr2))
  ) ++ (
  (* Cons (::) doit être traité différement des opérateurs binaires, car il possède un constructeur spécifique (ECons) *)
    p_token PARO *> parse_Expr *> p_token CONS *> parse_Expr *> p_token PARF >>= fun((((_, expr1), _), expr2), _) -> return (ECons(expr1,expr2))
  ) ++ (
  (* VIRG (,) doit être traité différement des opérateurs binaires, car il possède un constructeur spécifique (EProd) *)
    p_token PARO *> parse_Expr *> p_token VIRG *> parse_Expr *> p_token PARF >>= fun((((_, expr1), _), expr2), _) -> return (EProd(expr1,expr2))
  ) ++ (
    p_token PARO *> parse_Expr *> p_token PARF >>= fun ((_,expr1),_) -> return (expr1)
  ) ++ (
    p_token PARO *> parse_Expr *> parse_Expr *> p_token PARF >>= fun (((_,expr1), expr2),_) ->return  (EApply(expr1, expr2))
  ) ++ (
    p_token IF *> parse_Expr *> p_token THEN  *> parse_Expr *> p_token ELSE *> parse_Expr >>= fun (((((_,expr1), _), expr2), _), expr3) -> return (EIf(expr1, expr2, expr3))
  ) ++ (
    p_token PARO *> p_token FUN *> p_ident *> p_token TO *> parse_Expr *> p_token PARF >>= fun (((((_,_), ident), _), expr1), _) -> return (EFun(unident ident, expr1))
  ) ++ (
    p_ident >>= fun ident -> return (EIdent(unident ident))
  ) ++ (
    parse_Const >>= fun constant -> return (EConstant(constant))
  )
  ) flux

and parse_Binop flux =
  (
  (
    parse_Arithop 
  ) ++ (
    parse_Boolop
  ) ++ (
    parse_Relop
  ) ++ (
    p_token CONCAT >>= fun _ -> return (EBinop(CONCAT))
  )
  ) flux

and parse_Arithop flux =
  (
  (
    p_token PLUS >>= fun _ -> return (EBinop(PLUS))
  ) ++ (
    p_token MOINS >>= fun _ -> return (EBinop(MOINS))
  ) ++ (
    p_token MULT >>= fun _ -> return (EBinop(MULT))
  ) ++ (
    p_token DIV >>= fun _ -> return (EBinop(DIV))
  ) 
  ) flux

and parse_Boolop flux =
  (
  (
    p_token AND >>= fun _ -> return (EBinop(AND))
  ) ++ (
    p_token OR >>= fun _ -> return (EBinop(OR))
  )
  ) flux

and parse_Relop flux = 
  (
  (
    p_token EQU >>= fun _ -> return (EBinop(EQU))
  ) ++ (
    p_token NOTEQ >>= fun _ -> return (EBinop(NOTEQ))
  ) ++ (
    p_token INFEQ >>= fun _ -> return (EBinop(INFEQ))
  ) ++ (
    p_token INF >>= fun _ -> return (EBinop(INF))
  ) ++ (
    p_token INFEQ >>= fun _ -> return (EBinop(SUPEQ))
  ) ++ (
    p_token SUP >>= fun _ -> return (EBinop(SUP))
  )
  )flux

and parse_Const flux =
  (
  (
    p_int >>= fun integer -> return (CEntier(unint integer))
  ) ++ (
    p_bool >>= fun boolean -> return (CBooleen(unbool boolean))
  ) ++ (
    p_token CROO *> p_token CROF >>= fun (_,_) -> return (CNil)
  ) ++ (
    p_token PARO *> p_token PARF >>= fun (_,_) -> return (CUnit)
  )
  )flux

and p_ident = ptest isident
and p_int = ptest isint
and p_bool = ptest isbool

(* Parse a token*)
and p_token token = ptest (istoken token)
and istoken wantedToken token = (wantedToken == token)  

(* Write a solution*)
let rec print_solutions progs =
  match Solution.uncons progs with
  | None        -> ()
  | Some (p, q) ->
     begin
       Format.printf "\nExpression recognized: \n";
       Miniml_printer.print_expr Format.std_formatter p;
       print_solutions q
     end

(* Parsing a text*)
let rec parse_miniml flux = 
  let sol = run parse_Expr flux in
  match Solution.uncons sol with
    | None        -> failwith "parsing failed"
    | Some (p, q) ->
       begin
         p
       end

let print_expr e = 
  Miniml_printer.print_expr Format.std_formatter e
(*
and parse_miniml_interactif () = 
  let rec loop () =
    Format.printf "Write your miniml programme : \n";
    flush stdout;
    let line = read_line () in
    let flux = read_miniml_tokens_from_string line in
    let sol = parse_miniml flux in
    match Solution.uncons sol with
    | None        -> (Format.printf "\nparsing failed !\n"; loop ())
    | Some (p, q) ->
       begin
         print_solutions (Solution.cons p q);
         loop ()
      end
  in loop () 

and parse_miniml_file filename = 
    let flux = read_miniml_tokens_from_file filename in 
    let sol = parse_miniml flux in
    match Solution.uncons sol with
    | None        -> (Format.printf "\nparsing failed !\n")
    | Some (p, q) ->
       begin
         print_solutions (Solution.cons p q);
       end
*)


