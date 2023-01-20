open Miniml_types

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
module type EnvironmentSpec =
  sig
    (* type abstrait des variables      *)
    type t

    val gamma0 : t
    val add : t -> (expr * TypeVariable.t typ) -> t
    val typeOf : t -> expr -> TypeVariable.t typ
  end

module Environment : EnvironmentSpec = 
  struct
  type t = (expr * TypeVariable.t typ) list
  let gamma0 =
    let a = TypeVariable.fraiche() in
    let b = TypeVariable.fraiche() in
    [ 
    EBinop(CONCAT), TFun(TList(TVar(a)),TFun(TList(TVar(a)), TList(TVar(a))));
    EBinop(CONS), TFun(TVar(a), TFun(TList(TVar(a)), TList(TVar(a))));
    EBinop(VIRG), TFun(TVar(a), TFun(TVar(b), TProd(TVar(a), TVar(b))));
    EBinop(PLUS), TFun(TInt, TFun(TInt, TInt));
    EBinop(MOINS), TFun(TInt, TFun(TInt, TInt));
    EBinop(MULT), TFun(TInt, TFun(TInt, TInt));
    EBinop(DIV), TFun(TInt, TFun(TInt, TInt));
    EBinop(EQU), TFun(TVar(a), TFun(TVar(a), TBool));
    EBinop(NOTEQ), TFun(TVar(a), TFun(TVar(a), TBool));
    EBinop(INFEQ), TFun(TVar(a), TFun(TVar(a), TBool));
    EBinop(INF), TFun(TVar(a), TFun(TVar(a), TBool));
    EBinop(SUPEQ), TFun(TVar(a), TFun(TVar(a), TBool));
    EBinop(SUP), TFun(TVar(a), TFun(TVar(a), TBool));
    EBinop(AND), TFun(TBool, TFun(TBool, TBool));
    EBinop(OR), TFun(TBool, TFun(TBool, TBool));
    EIdent("not"), TFun(TBool, TBool);
    EIdent("fst"), TFun(TProd(TVar(a),TVar(b)), TVar(a));
    EIdent("snd"), TFun(TProd(TVar(a),TVar(b)), TVar(b));
    EIdent("hd"), TFun(TList(TVar(a)), TVar(a));
    EIdent("tl"), TFun(TList(TVar(a)), TList(TVar(a)))
    ]
  
  let add env couple = couple::env
  
  let copy : TypeVariable.t typ -> TypeVariable.t typ = fun t ->
    let rec internal t ls =
      match t with
      | TFun(a, b) -> begin match internal a ls with (t1, ls1) -> match internal b ls1 with (t2, ls2) -> (TFun(t1, t2), ls2) end
      | TProd(a, b) -> begin match internal a ls with (t1, ls1) -> match internal b ls1 with (t2, ls2) -> (TProd(t1, t2), ls2) end
      | TList(a) -> let (t1, ls1) = internal a ls in (TList(t1), ls1)   
      | TVar(a) -> begin match List.assoc_opt a ls with Some(b) -> (TVar(b), ls) | None -> let vf = TypeVariable.fraiche () in (TVar(vf), (a, vf)::ls) end
      | a -> a, ls
    in match internal t [] with (t, _) -> t


  let rec typeOf env expr =
    let rec internal en =
      match en with 
        | [] -> failwith "typeOf"
        | (e, t)::q -> if e = expr then t else internal q
    in let t = internal env in match t with TFun(_, _) -> copy t | a -> a
end

let rec replace typeOld typeNew typ =
  match typ with |TVar(a) -> if (TypeVariable.equal a typeOld) then typeNew else typ
                 |TProd(a,b) -> TProd(replace typeOld typeNew a, replace typeOld typeNew b)
                 |TList(a) -> TList(replace typeOld typeNew a)
                 |TFun(a,b) -> TFun(replace typeOld typeNew a, replace typeOld typeNew b)
                 |TBool -> TBool
                 |TInt -> TInt
                 |TUnit -> TUnit

module type EquationTypeSpec =
sig
  type t
  val empty : t
  val add : t -> (TypeVariable.t typ * TypeVariable.t typ ) -> t
  val map : (TypeVariable.t typ -> TypeVariable.t typ) -> t -> t
  val replace_all : TypeVariable.t -> TypeVariable.t typ -> t -> t
  val hd : t -> (TypeVariable.t typ * TypeVariable.t typ) option
  val tl : t -> t
end

module EquationType : EquationTypeSpec =
struct
  type t = (TypeVariable.t typ * TypeVariable.t typ) list
  let empty = []
  let add equ couple = couple::equ
  let map f equ = List.map (fun (a,b) -> (f a, f b)) equ
  let replace_all typeOld typeNew equ =
     List.map (fun (a,b) -> (replace typeOld typeNew a, replace typeOld typeNew b)) equ
  let hd equ = match equ with |[] -> None
                              |a::q -> Some(a)
  let tl equ = match equ with |[] -> []
                              |a::q -> q
end

let rec type_rule : Environment.t -> EquationType.t -> expr -> (EquationType.t * TypeVariable.t typ) =
  fun env equ expr ->
  match expr with 
    | EConstant(constant) -> begin match constant with 
                              |CBooleen(_) -> equ, TBool
                              |CEntier(_) -> equ, TInt
                              |CUnit -> equ, TUnit
                              |CNil -> equ, TList(TVar(TypeVariable.fraiche ()))
                             end
    (* variable                    *)
    | EIdent(_)            -> equ, Environment.typeOf env expr
    (* paire (e1, e2)              *)
    | EProd(expr1, expr2)  -> let (equ1, type1) = type_rule env equ expr1 in
                              let (equ2, type2) = type_rule env equ1 expr2 in
                              equ2, TProd(type1, type2)
    (* liste (e1::e2)              *)
    | ECons(expr1, expr2)  ->   let (equ1, type1) = type_rule env equ expr1 in
                                let (equ2, type2) = type_rule env equ1 expr2 in
                                EquationType.add equ2 (type2, TList(type1)), type2
    (* fonction anonyme            *)
    | EFun(ident, expr) -> let tident = TVar(TypeVariable.fraiche()) in
                           let (equ1, type1) = type_rule (Environment.add env (EIdent(ident),  tident)) equ expr in
                           equ1, TFun(tident, type1)
    (* conditionnelle              *)
    | EIf(boolean, expr1, expr2) -> let (equ1, type1) = type_rule env equ boolean in
                                    let (equ2, type2) = type_rule env equ1 expr1 in
                                    let (equ3, type3) = type_rule env equ2 expr2 in
                                    EquationType.add (EquationType.add equ3 (type1, TBool) ) (type2, type3), type3
    (* application (e1 e2)         *)
    | EApply(expr1, expr2) -> let (equ1, type1) = type_rule env equ expr1 in
                              let (equ2, type2) = type_rule env equ1 expr2 in
                              let tident = TVar(TypeVariable.fraiche()) in
                              EquationType.add equ2 (type1, TFun(type2, tident)), tident

    (* opérateur binaire prédéfini *)
    | EBinop(op) -> equ, Environment.typeOf env expr
    (* définition locale           *)
    | ELet(ident, expr1, expr2) -> let (equ1, type1) = type_rule env equ expr1 in
                                   let (equ2, type2) = type_rule (Environment.add env (EIdent(ident), type1)) equ1 expr2 in
                                   equ2, type2
    (* définition récursive locale *)
    | ELetrec(ident, expr1, expr2) -> let tident = TVar(TypeVariable.fraiche()) in
                                      let (equ1, type1) = type_rule (Environment.add env (EIdent(ident), tident)) equ expr1 in
                                      let (equ2, type2) = type_rule (Environment.add env (EIdent(ident), type1)) equ1 expr2 in
                                      EquationType.add equ2 (type1, tident), type2

let rec normalize_rule : EquationType.t -> TypeVariable.t typ -> TypeVariable.t typ =
  fun equ result ->
    match EquationType.hd equ with
      |Some(TBool, TBool) -> normalize_rule (EquationType.tl equ) result 
      |Some(TUnit, TUnit) -> normalize_rule (EquationType.tl equ) result
      |Some(TList(type1), TList(type2)) -> normalize_rule (EquationType.add equ (type1,type2)) result
      |Some(TFun(type1, type2), TFun(type3, type4)) -> normalize_rule (EquationType.add (EquationType.add equ (type1, type3)) (type2, type4)) result
      |Some(TProd(type1, type2), TProd(type3, type4)) -> normalize_rule (EquationType.add (EquationType.add equ (type1, type3)) (type2, type4)) result
      |Some(TVar(type1), TVar(type2)) -> if (TypeVariable.equal type1 type2) 
                                     then normalize_rule (EquationType.tl equ) result
                                     else normalize_rule (EquationType.replace_all type1 (TVar(type2)) (EquationType.tl equ)) (replace type1 (TVar(type2)) result)

      |Some(TVar(type1), type2) -> normalize_rule (EquationType.replace_all type1 type2 (EquationType.tl equ)) (replace type1 type2 result)
      |Some(type1, TVar(type2)) -> normalize_rule (EquationType.add (EquationType.tl equ) (TVar(type2),type1) ) result
      |None -> result
      |_ -> failwith "Erreur de typage"

let type_of : expr -> TypeVariable.t typ =
  fun expr ->
    let (equ, t) = type_rule Environment.gamma0 EquationType.empty expr in
    normalize_rule equ t 

let rec print_types t =
  Format.printf " : ";
  Miniml_printer.print_typ TypeVariable.fprintf Format.std_formatter t;
  Format.printf "\n";
