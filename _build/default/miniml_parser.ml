open Miniml_types
open Miniml_lexer
open Lazyflux
open Typeparser

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


let iseof =
  function EOF     -> true
         | _           -> false
let islet =
  function LET     -> true
         | _           -> false
let isin =
  function IN     -> true
         | _           -> false
let isrec =
  function REC     -> true
         | _           -> false
let ismatch =
  function MATCH     -> true
         | _           -> false
let iswith =
  function WITH     -> true
         | _           -> false
let isbar =
  function BAR     -> true
         | _           -> false
let isfun =
  function FUN     -> true
         | _           -> false
let isto =
  function TO     -> true
         | _           -> false
let isif =
  function IF     -> true
         | _           -> false
let isthen =
  function THEN     -> true
         | _           -> false
let iselse =
  function ELSE     -> true
         | _           -> false
let isdoubleptvirg =
  function DOUBLEPTVIRG     -> true
         | _           -> false
let issep =
  function SEP     -> true
         | _           -> false
let iscons =
  function CONS     -> true
         | _           -> false
let isconcat =
  function CONCAT     -> true
         | _           -> false
let isparo =
  function PARO     -> true
         | _           -> false
let isparf =
  function PARF     -> true
         | _           -> false
let iscroo =
  function CROO     -> true
         | _           -> false
let iscrof =
  function CROF     -> true
         | _           -> false
let isptvirg =
  function PTVIRG     -> true
         | _           -> false
let isvirg =
  function VIRG     -> true
         | _           -> false
let isplus =
  function PLUS     -> true
         | _           -> false
let ismoins =
  function MOINS     -> true
         | _           -> false
let isumoins =
  function UMOINS     -> true
         | _           -> false
let ismult =
  function MULT     -> true
         | _           -> false
let isdiv =
  function DIV     -> true
         | _           -> false
let ismod =
  function MOD     -> true
         | _           -> false
let isfplus =
  function FPLUS     -> true
         | _           -> false
let isfmoins =
  function FMOINS     -> true
         | _           -> false
let isfumoins =
  function FUMOINS     -> true
         | _           -> false
let isfmult =
  function FMULT     -> true
         | _           -> false
let isfdiv =
  function FDIV     -> true
         | _           -> false
let isequ =
  function EQU     -> true
         | _           -> false        
let isinfeq =
  function INFEQ     -> true
         | _           -> false
let issupeq =
  function SUPEQ     -> true
         | _           -> false
let isinf =
  function INF     -> true
         | _           -> false
let issup =
  function SUP     -> true
         | _           -> false
let isnoteq =
  function NOTEQ     -> true
         | _           -> false
let isand =
  function AND     -> true
         | _           -> false
let isor =
  function OR     -> true
         | _           -> false
let isfloat =
  function FLOAT _     -> true
         | _           -> false
let isconstruct =
  function CONSTRUCT _     -> true
         | _           -> false
let istype =
  function TYPE     -> true
         | _           -> false
let isquote =
  function QUOTE     -> true
         | _           -> false


open Parser

let p_int = ptest isint;; 
let p_bool = ptest isbool;;
let p_ident = ptest isident;;

let p_eof = ptest iseof;;
let p_let = ptest islet;;
let p_in = ptest isin;;
let p_rec = ptest isrec;;
let p_match = ptest ismatch;;
let p_with = ptest iswith;;
let p_bar = ptest isbar;;
let p_fun = ptest isfun;;
let p_to = ptest isto;;
let p_if = ptest isif;;
let p_then = ptest isthen;;
let p_else = ptest iselse;;
let p_doubleptvirg = ptest isdoubleptvirg;;
let p_sep = ptest issep;;
let p_cons = ptest iscons;;
let p_concat = ptest isconcat;;
let p_paro = ptest isparo;;
let p_parf = ptest isparf;;
let p_croo = ptest iscroo;;
let p_crof = ptest iscrof;;
let p_ptvirg = ptest isptvirg;;
let p_virg = ptest isvirg;;
let p_plus = ptest isplus;;
let p_moins = ptest ismoins;;
let p_umoins = ptest isumoins;;
let p_mult = ptest ismult;;
let p_div = ptest isdiv;;
let p_mod = ptest ismod;;
let p_fplus = ptest isfplus;;
let p_fmoins = ptest isfmoins;;
let p_fumoins = ptest isfumoins;;
let p_fmult = ptest isfmult;;
let p_fdiv = ptest isfdiv;;
let p_equ = ptest isequ;;
let p_infeq = ptest isinfeq;;
let p_supeq = ptest issupeq;;
let p_inf = ptest isinf;;
let p_sup = ptest issup;;
let p_noteq = ptest isnoteq;;
let p_and = ptest isand;;
let p_or = ptest isor;;
let p_float = ptest isfloat;;
let p_construct = ptest isconstruct;;
let p_type = ptest istype;;
let p_quote = ptest isquote;;


(* let drop p = map (fun x -> ()) p;;


let is_space c = String.contains " \t\r\n" c;;


let space  = drop (ptest is_space);;


let rec eat_space p flux =
  (map snd (space *> (eat_space p)) ++ p) flux;;

let p_car c = drop (ptest ((=) c));;

let p_chaine s =
  let rec parse i =
    if i < 0
    then return ()
    else map fst (parse (i-1) *> p_car s.[i])
  in parse (String.length s - 1)


let p_eof = eat_space pvide;;

let p_to  = eat_space (p_chaine "->");;
let p_doubleptvirg  = eat_space (p_chaine ";;");;
let p_cons  = eat_space (p_chaine "::");;
let p_plus  = eat_space (p_car '+');;
let p_moins  = eat_space (p_car '-');;
let p_mult  = eat_space (p_car '*');;
let p_div  = eat_space (p_car '/');;
let p_fplus  = eat_space (p_chaine "+.");;
let p_fmoins  = eat_space (p_chaine "-.");;
let p_fmult  = eat_space (p_chaine "*.");;
let p_fdiv  = eat_space (p_chaine "/.");;
let p_parf  = eat_space (p_car ')');;
let p_croo  = eat_space (p_car '[');;
let p_crof  = eat_space (p_car ']');;
let p_equ  = eat_space (p_car '=');;
let p_noteq  = eat_space (p_chaine "<>");;
let p_infeq = eat_space (p_chaine "<=");;
let p_supeq  = eat_space (p_chaine ">=");;
let p_inf  = eat_space (p_car '<');;
let p_sup  = eat_space (p_car '>');;
let p_concat  = eat_space (p_car '@');;
let p_and  = eat_space (p_chaine "&&");;
let p_or  = eat_space (p_chaine "||");;
let p_vig  = eat_space (p_car ',');;
let p_ptvirg  = eat_space (p_car ';');;
let p_true  = eat_space (p_chaine "true");;
let p_false  = eat_space (p_chaine "false");;
let p_mod  = eat_space (p_chaine "mod");;
let p_let  = eat_space (p_chaine "let");;
let p_in  = eat_space (p_chaine "in");;
let p_rec  = eat_space (p_chaine "rec");;
let p_fun  = eat_space (p_chaine "fun");;
let p_match  = eat_space (p_chaine "match");;
let p_with  = eat_space (p_chaine "with");;
let p_bar  = eat_space (p_car '|');;
let p_if  = eat_space (p_chaine "if");;
let p_then  = eat_space (p_chaine "then");;
let p_else  = eat_space (p_chaine "else");; *)






let rec parseExpr : (token, expr) parser = fun flux ->
  (
    (p_let *> p_ident *> p_equ *> parseExpr *> p_in *> parseExpr >>= fun(((((_, id), _), expr1), _), expr2) -> return (ELet(unident id, expr1, expr2))) ++
    (p_let *> p_rec *> p_ident *> p_equ *> parseExpr *> p_in *> parseExpr >>= fun((((((_, _), id), _), e1), _), e2) -> return (ELetrec(unident id, e1, e2))) ++
    (* 3 Prochaine ligne a revoir *)
    (p_paro *> parseExpr *> parseBinop *> parseExpr *> p_parf >>= fun((((_, e1), op), e2), _) -> return (EApply(EApply(op, e1), e2))) ++
    (p_paro *> parseExpr *> p_cons *> parseExpr *> p_parf >>= fun((((_, e1), _), e2), _) -> return (ECons(e1, e2))) ++
    (p_paro *> parseExpr *> p_virg *> parseExpr *> p_parf >>= fun((((_, e1), _), e2), _) -> return (EProd(e1, e2))) ++
    (p_paro *> parseExpr *> p_parf >>= fun ((_, e1), _) -> return e1) ++ 
    (* 3 derniere ligne a revoir *)
    (p_paro *> parseExpr *> parseExpr *> p_parf >>= fun (((_, e1), e2), _) -> return (EApply(e1, e2))) ++
    (p_if *> parseExpr *> p_then *> parseExpr *> p_else *> parseExpr >>= fun (((((_, e1), _), e2), _), e3) -> return (EIf(e1, e2, e3))) ++
    (p_paro *> p_fun *> p_ident *> p_to *> parseExpr *> p_parf >>= fun(((((_, _),i), _), expr), _) -> return (EFun(unident i, expr))) ++
    (p_ident >>= fun i -> return (EIdent(unident i))) ++
    (parseConstant >>= fun c -> return (EConstant(c)))
  ) flux
and parseConstant : (token, constant) parser = fun flux ->
  (
    (p_int >>= fun i -> return (CEntier(unint i))) ++
    (p_bool >>= fun b -> return (CBooleen(unbool b))) ++
    (p_croo *> p_crof >>= fun (_, _) -> return (CNil)) ++
    (p_paro *> p_parf >>= fun (_, _) -> return (CUnit))
  ) flux
and parseBinop : (token, expr) parser = fun flux ->
  (
    (parseArithop) ++
    (parseBoolop) ++ 
    (parseRelop) ++
    (p_concat >>= fun _ -> return (EBinop(CONCAT)))
  ) flux
and parseArithop : (token, expr) parser = fun flux -> 
  (
    (p_plus >>= fun _ -> return (EBinop(PLUS))) ++
    (p_moins >>= fun _ -> return (EBinop(MOINS))) ++ 
    (p_mult >>= fun _ -> return (EBinop(MULT))) ++ 
    (p_div >>= fun _ -> return (EBinop(DIV)))
  ) flux
and parseBoolop : (token, expr) parser = fun flux ->
  (
    (p_and >>= fun _ -> return (EBinop(AND))) ++
    (p_or >>= fun _ -> return (EBinop(OR)))
  ) flux
and parseRelop : (token, expr) parser = fun flux -> 
  (
    (p_equ >>= fun _ -> return (EBinop(EQU))) ++
    (p_noteq >>= fun _ -> return (EBinop(NOTEQ))) ++
    (p_infeq >>= fun _ -> return (EBinop(INFEQ))) ++ 
    (p_inf >>= fun _ -> return (EBinop(INF))) ++
    (p_supeq >>= fun _ -> return (EBinop(SUPEQ))) ++
    (p_sup >>= fun _ -> return (EBinop(SUP)))
  ) flux




(* let flux_of_token s =
  let f = ref Flux.vide in
  let lexbuf  = (Lexing.from_string s) in
  let token = ref (Miniml_lexer.token (lexbuf)) in 
  while ((!token) != EOF) do
    (f := Flux.cons !token !f);
    (token := (Miniml_lexer.token lexbuf))
  done *)

let parse_miniml flux = run parseExpr flux;;

let rec print_solutions expr =
  match Solution.uncons expr with
  | None        -> ()
  | Some (p, q) ->
     begin
       Format.printf "Expression recognized: \n";
       Miniml_printer.print_expr Format.std_formatter p;
       Format.printf "\n";
       print_solutions q
     end;;


let test_parser_logo () =
  let rec loop () =
    Format.printf "programme?@.";
    flush stdout;
    let l = read_line () in
    let f = read_miniml_tokens_from_string l in
    let progs = parse_miniml f in
    match Solution.uncons progs with
    | None        -> (Format.printf "** parsing failed ! **@."; loop ())
    | Some (p, q) ->
       begin
         print_solutions (Solution.cons p q);
         loop ()
      end
  in loop ();;

let parse_single_expr str =
    let f = read_miniml_tokens_from_string str in
    let progs = parse_miniml f in
    match Solution.uncons progs with
    | None        -> failwith "** parsing failed ! **@."
    | Some (p, _) -> p