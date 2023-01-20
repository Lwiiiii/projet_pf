open Miniml_types
open Miniml_parser

exception Typing_Exception of string

(* signature minimale pour définir des variables *)
module type VariableSpec =
  sig
    (* type abstrait des variables      *)
    type t

    (* création d'une variable fraîche  *)
    val fraiche : unit -> t

    (* fonctions de comparaison         *)
    (* permet de définir des conteneurs *)
    (* (hash-table, etc) de variables   *)
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val hash : t -> int

    (* fonction d'affichage             *)
    (* on utilise Format.std_formatter  *)
    (* comme premier paramètre          *)
    (* pour la sortie standard          *) 
    val fprintf : Format.formatter -> t -> unit
  end

(* implantation de la spécification     *)
module TypeVariable : VariableSpec =
  struct
    type t = int

    let fraiche =
      let cpt = ref 0 in
      (fun () -> incr cpt; !cpt)

    let compare a b = a - b
    let equal a b = a = b
    let hash a = Hashtbl.hash a

    let fprintf fmt a = Format.fprintf fmt "t{%d}" a
  end


(* ******** à compléter ********* *)
module type EnvSpec =
  sig
  type t

  val create : unit -> t
  val copy : TypeVariable.t typ -> TypeVariable.t typ 
  val add : expr -> TypeVariable.t typ -> t -> t
  val getType : t -> expr -> TypeVariable.t typ 
  end

module Environment : EnvSpec =
  struct
  type t = (expr * TypeVariable.t typ) list

  let create () = 
    let typeA = TypeVariable.fraiche () and
    typeB = TypeVariable.fraiche () in 
    let concatOp = (EBinop(CONCAT), TFun(TList(TVar(typeA)), TFun(TList(TVar(typeA)), TList(TVar(typeA)))))
    and consOp = (EBinop(CONS), TFun(TVar(typeA), TFun(TList(TVar(typeA)), TList(TVar(typeA)))))
    and pairOp = (EBinop(VIRG), TFun(TVar(typeA), TFun(TVar(typeB), TVar(typeA))))
    and plusOp = (EBinop(PLUS), TFun(TInt, TFun(TInt, TInt)))
    and moinsOp = (EBinop(MOINS), TFun(TInt, TFun(TInt, TInt)))
    and multOp = (EBinop(MULT), TFun(TInt, TFun(TInt, TInt)))
    and divOp = (EBinop(DIV), TFun(TInt, TFun(TInt, TInt)))
    and eqOp = (EBinop(EQU), TFun(TVar(typeA), TFun(TVar(typeA), TBool)))
    and noteqOp = (EBinop(NOTEQ), TFun(TVar(typeA), TFun(TVar(typeA), TBool)))
    and infeqOp = (EBinop(INFEQ), TFun(TVar(typeA), TFun(TVar(typeA), TBool)))
    and infOp = (EBinop(INF), TFun(TVar(typeA), TFun(TVar(typeA), TBool)))
    and supeqOp = (EBinop(SUPEQ), TFun(TVar(typeA), TFun(TVar(typeA), TBool)))
    and supOp = (EBinop(SUP), TFun(TVar(typeA), TFun(TVar(typeA), TBool)))
    and andOp = (EBinop(AND), TFun(TBool, TFun(TBool, TBool)))
    and orOp = (EBinop(OR), TFun(TBool, TFun(TBool, TBool)))
    and notFunc = (EIdent("not"), TFun(TBool, TBool))
    and fstFunc = (EIdent("fst"), TFun(TProd(TVar(typeA), TVar(typeB)), TVar(typeA))) 
    and sndFunc = (EIdent("snd"), TFun(TProd(TVar(typeA), TVar(typeB)), TVar(typeB)))
    and hdFunc = (EIdent("hd"), TFun(TList(TVar(typeA)), TVar(typeA)))
    and tlFunc = (EIdent("tl"), TFun(TList(TVar(typeA)), TList(TVar(typeA)))) in
    
    [concatOp; consOp; pairOp; plusOp; moinsOp; multOp; divOp; eqOp; noteqOp; infeqOp; infOp; supeqOp; supOp; andOp; orOp;
    notFunc; fstFunc; sndFunc; hdFunc; tlFunc] 

  let copy : TypeVariable.t typ -> TypeVariable.t typ = fun t ->
    let rec internal t ls =
      match t with
      | TFun(a, b) -> begin match internal a ls with (t1, ls1) -> match internal b ls1 with (t2, ls2) -> (TFun(t1, t2), ls2) end
      | TProd(a, b) -> begin match internal a ls with (t1, ls1) -> match internal b ls1 with (t2, ls2) -> (TProd(t1, t2), ls2) end
      | TList(a) -> let (t1, ls1) = internal a ls in (TList(t1), ls1)   
      | TVar(a) -> begin match List.assoc_opt a ls with Some(b) -> (TVar(b), ls) | None -> let vf = TypeVariable.fraiche () in (TVar(vf), (a, vf)::ls) end
      | a -> a, ls
    in match internal t [] with (t, _) -> t

  let getType : t -> expr -> TypeVariable.t typ = fun env expr ->
    let rec internal en =
      match en with 
        | [] -> raise (Typing_Exception("Type to expression not found!"))
        | (e, t)::q -> if e = expr then t else internal q
    in let t = internal env in match t with TFun(_, _) -> copy t | a -> a

    let add : expr -> TypeVariable.t typ -> t -> t = fun exp t env -> (exp, t)::env 
  end

let rec replace_typevar : TypeVariable.t typ -> TypeVariable.t -> TypeVariable.t typ -> TypeVariable.t typ =
  fun ti tvar trep -> match ti with
    | TProd(a, b) -> TProd(replace_typevar a tvar trep, replace_typevar b tvar trep)
    | TFun(a, b) -> TFun(replace_typevar a tvar trep, replace_typevar b tvar trep)
    | TList(a) -> TList(replace_typevar a tvar trep)
    | TVar(a) -> if TypeVariable.equal a tvar then trep else TVar(a)
    | a -> a  

module type EquSpec = 
  sig 
  type t

  val create : unit -> t
  val cons : TypeVariable.t typ -> TypeVariable.t typ -> t -> t
  val uncons : t -> ((TypeVariable.t typ * TypeVariable.t typ) * t) option
  val replace : TypeVariable.t -> TypeVariable.t typ -> t -> t
  end

module Equations : EquSpec = 
  struct 
  type t = (TypeVariable.t typ * TypeVariable.t typ) list

  let create () = [] 
  let cons : TypeVariable.t typ -> TypeVariable.t typ -> t -> t = fun t1 t2 ls -> (t1,t2)::ls
  let uncons : t -> ((TypeVariable.t typ * TypeVariable.t typ) * t) option = function
    | [] -> None
    | a::b -> Some(a,b)

  let rec replace : TypeVariable.t -> TypeVariable.t typ -> t -> t = 
    fun tvar tnew eqs -> match eqs with
      | [] -> []
      | (t1, t2)::b -> (replace_typevar t1 tvar tnew, replace_typevar t2 tvar tnew)::(replace tvar tnew b)
  end
(*
let getType : (TypeVariable.t * TypeVariable.t typ) list -> TypeVariable.t -> TypeVariable.t typ option =
  fun l tv -> List.assoc_opt tv l

let tryConvert : TypeVariable.t typ -> TypeVariable.t typ -> (TypeVariable.t * TypeVariable.t typ) list option = fun t1 t2 ->
  let rec loop (t1,tb1) (t2,tb2) = match t1, t2 with
    | TFun(a, b), TFun(c, d) | TProd(a, b), TProd(c, d) -> begin match loop (a,tb1) (c,tb2) with (tbl1, tbl2) -> loop (b,tbl1) (d,tbl2) end
    | TList(a), TList(b) -> loop (a,tb1) (b,tb2)
    | TVar(a), b -> if (List.assoc a tbl) =


let rec replace : TypeVariable.t typ -> TypeVariable.t -> TypeVariable.t typ -> TypeVariable.t typ =
    fun init vt t -> match init with
      | TFun(a, b) -> TFun(replace a vt t, replace b vt t)
      | TList(a) -> TList(replace a vt t)
      | TProd(a, b) -> TProd(replace a vt t, replace b vt t)
      | TVar(a) -> if TypeVariable.equal a vt then t else TVar(a)
      | c -> c

let rec isCompatible : TypeVariable.t typ -> TypeVariable.t typ -> bool =
  fun t1 t2 -> match t1, t2 with
    | TFun(a, b), TFun(TVar(c), d) | TProd(a, b), TProd(TVar(c), d) -> isCompatible b (replace d c a)
    | TFun(TVar(a), b), TFun(c, d) | TProd(TVar(a), b), TProd(c, d) -> isCompatible d (replace b a c)
    | TFun(a, b), TFun(c, d) | TProd(a, b), TProd(c, d) -> isCompatible a c && isCompatible b d
    | TList(a), TList(b) -> isCompatible a b
    | _, TVar(_) -> true
    | a, b -> a = b
*)

let rec containsTVar : TypeVariable.t -> TypeVariable.t typ -> bool = 
  fun tvar t -> match t with    
    | TProd(a, b) | TFun(a, b) -> containsTVar tvar a || containsTVar tvar b
    | TList(a) -> containsTVar tvar a
    | TVar(a) -> TypeVariable.equal tvar a
    | _ -> false


let rec processExpr : expr -> Environment.t -> Equations.t -> (TypeVariable.t typ * Environment.t * Equations.t) = 
    fun exp env eq-> 
      match exp with
      | ELet(id, e1, e2) -> let (t1, _, eq1) = processExpr e1 env eq in 
                            let env1 = Environment.add (EIdent(id)) t1 env 
                            in processExpr e2 env1 eq1
      | ELetrec(id, e1, e2) ->  let tf = TVar(TypeVariable.fraiche ()) in
                                let env1 = Environment.add (EIdent(id)) tf env in
                                let (t1, _, eq1) = processExpr e1 env1 eq in 
                                processExpr e2 env1 (Equations.cons tf t1 eq1)
      | EApply(e1, e2) -> let (t1, _, eq1) = processExpr e1 env eq in
                          let (t2, _, eq2) = processExpr e2 env eq1 in
                          let tapp = TVar(TypeVariable.fraiche ()) in
                          (tapp, env, Equations.cons t1 (TFun(t2, tapp)) eq2)
      | ECons(e1, e2) -> let (t1, _, eq1) = processExpr e1 env eq in
                         let (t2, _, eq2) = processExpr e2 env eq1 in
                         (t2, env, Equations.cons t2 (TList(t1)) eq2) 
      | EIf(e1, e2, e3) ->  let (t1, _, eq1) = processExpr e1 env eq in
                            let (t2, _, eq2) = processExpr e2 env eq1 in
                            let (t3, _, eq3) = processExpr e3 env eq2 in
                            (t2, env, Equations.cons t2 t3 (Equations.cons t1 TBool eq3))
      | EFun(id, e) ->  let tf = TVar(TypeVariable.fraiche ()) in
                        let (t1, _, eq1) = processExpr e (Environment.add (EIdent(id)) tf env) eq in
                        (TFun(tf, t1), env, eq1)
                        
      | EConstant(c) -> let cType = match c with 
          | CBooleen(_) -> TBool
          | CEntier(_) -> TInt
          | CUnit -> TUnit
          | CNil -> TList(TVar(TypeVariable.fraiche ()))
          in cType, env, eq
      | EProd(e1, e2) ->  let (t1, _, eq1) = processExpr e1 env eq in 
                          let (t2, _, eq2) = processExpr e2 env eq1 in 
                          TProd(t1, t2), env, eq2
      | EIdent(_) -> Environment.getType env exp, env, eq
      | EBinop(_) -> Environment.getType env exp, env, eq

let rec processEquations : Equations.t -> TypeVariable.t typ -> (Equations.t * TypeVariable.t typ) = 
    fun eqs t -> match Equations.uncons eqs with
      | Some((t1, t2), q) -> begin match t1, t2 with
          | TBool, TBool | TInt, TInt | TUnit, TUnit -> processEquations q t
          | TList(a), TList(b) -> processEquations (Equations.cons a b q) t
          | TFun(a, b), TFun(c, d) | TProd(a, b), TProd(c, d) -> processEquations (Equations.cons a c (Equations.cons b d q)) t
          | TVar(a), TVar(b) -> if TypeVariable.equal a b then processEquations q t else processEquations (Equations.replace a t2 q) (replace_typevar t a t2)
          | TVar(a), b -> if containsTVar a b then raise (Typing_Exception "Found recursive type...") else processEquations (Equations.replace a b q) (replace_typevar t a t2)
          | _, TVar(_) -> processEquations (Equations.cons t2 t1 q) t
          | _, _ -> raise (Typing_Exception("Unknown type equation..."))
          end
      | None -> (eqs, t)

let computeType : expr -> TypeVariable.t typ = 
  fun exp -> let (t, _, eqs) = processExpr exp (Environment.create ()) (Equations.create ())
    in let (_, t2) = processEquations eqs t
    in t2

open Typeparser
let rec test () =
  let vf1 = TVar(TypeVariable.fraiche ()) and vf2 = TVar(TypeVariable.fraiche ()) and vf3 = TVar(TypeVariable.fraiche ()) and vf4 = TVar(TypeVariable.fraiche ()) in 
  let eqs = Equations.cons (TFun(TList(vf1), vf1)) (TFun(TList(vf2), vf3)) (Equations.cons (TList(vf2)) (TList(TInt)) (Equations.create ()))
  in processEquations eqs vf3

let rec loop () =
  Format.printf "programme?@.";
  flush stdout;
  let l = read_line () in
  let f = read_miniml_tokens_from_string l in
  let progs = parse_miniml f in
  match Solution.uncons progs with
  | None        -> failwith "F"
  | Some (p, q) -> processExpr p (Environment.create ()) (Equations.create ())

let rec print_solutions expr =
  match Solution.uncons expr with
  | None        -> ()
  | Some (p, q) ->
     begin
       Miniml_printer.print_expr Format.std_formatter p;
    (try let t = computeType p in
       Format.printf " : ";
       Miniml_printer.print_typ TypeVariable.fprintf Format.std_formatter t;
       Format.printf "\n";
    with Typing_Exception(msg) -> Format.printf ": Could not find type (%s)" msg;);
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