open Ast

module type Monad = sig
  type 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
  val pure : 'a -> 'a t
  val liftA2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

(** Monad helper functions *)
module FuncM (M : Monad) = struct
  include M

  let ( let* ) = bind
  let ( <$> ) = map
  let ( <$ ) v m = map (fun _ -> v) m
  let ( >>= ) = bind
end

module OptM = FuncM (struct
  type 'a t = 'a option

  let map = Option.map
  let pure x = Some x

  let liftA2 f m1 m2 =
    match (m1, m2) with Some v1, Some v2 -> Some (f v1 v2) | _ -> None

  let bind m f = match m with Some v -> f v | None -> None
end)

module LocMap = Map.Make (Int)
module Env = Map.Make (String)
module Object = Map.Make (String)

type primitive = PInt of int | PString of string | PBool of bool | PUndef
type location = int
type clos_type = ClRegular | ClFix of id

type value =
  | VPrim of primitive
  | VLoc of location
  | VClosure of closure
  | VObject of obj
  | VExtern of ext_fun

and env = value Env.t

and closure = {
  cl_typ : clos_type;
  cl_env : env;
  cl_args : id list;
  cl_body : expr;
}

and obj = value Object.t

and ext_fun =
  | ExtUnary of (value -> value)
  | ExtBinary of (value -> value -> value)

let object_of_fields fs = Object.of_seq (List.to_seq fs)

type 'a except = RValue of 'a | RException of value
type state = { loc_cnt : location; loc_map : value LocMap.t }

module StateM = struct
  include FuncM (struct
    type 'a t = state -> 'a * state

    let map f m s =
      let v, s1 = m s in
      (f v, s1)

    let pure x s = (x, s)

    let liftA2 f m1 m2 s =
      let v1, s1 = m1 s in
      let v2, s2 = m2 s1 in
      (f v1 v2, s2)

    let bind m f s =
      let v1, s1 = m s in
      f v1 s1
  end)

  let gets f s = (f s, s)
  let state f s = f s
  let run_state m s = m s
end

module ExceptionT (M : Monad) = struct
  include FuncM (struct
    type 'a t = 'a except M.t

    let map f =
      M.map (function RValue v -> RValue (f v) | RException _ as e -> e)

    let pure x = M.pure (RValue x)

    let liftA2 f =
      M.liftA2 (fun m1 m2 ->
          match (m1, m2) with
          | RValue v1, RValue v2 -> RValue (f v1 v2)
          | RException e, _ -> RException e
          | _, RException e -> RException e)

    let bind m f =
      M.bind m (function RValue v -> f v | RException _ as e -> M.pure e)
  end)

  let lift m = M.map (fun v -> RValue v) m
  let throw e = M.pure (RException e)
  let run_exn m = m

  let catch m f =
    M.bind m (function RValue v -> M.pure (RValue v) | RException e -> f e)

  let catch_finally m h f =
    M.bind m (function
      | RValue v -> v <$ f
      | RException e -> bind (h e) (fun r -> r <$ f))
end

module ReaderT (M : Monad) = struct
  include FuncM (struct
    type 'a t = env -> 'a M.t

    let map f m e = M.map f (m e)
    let pure v _ = M.pure v
    let liftA2 f m1 m2 e = M.liftA2 f (m1 e) (m2 e)
    let bind m f e = M.bind (m e) (fun a -> f a e)
  end)

  let asks f e = M.pure (f e)
  let local f m e = m (f e)
  let lift m _ = m
  let run_reader m r = m r
end

module ResultM = ExceptionT (StateM)

type result = value except

let string_exn s = ResultM.throw (VPrim (PString s))

let initial_env =
  Env.of_seq
    (List.to_seq
       [
         ( "is_int",
           ExtUnary
             (function VPrim (PInt _) as v -> v | _ -> VPrim (PBool false)) );
         ( "is_bool",
           ExtUnary
             (function VPrim (PBool _) as v -> v | _ -> VPrim (PBool false)) );
         ( "is_string",
           ExtUnary
             (function VPrim (PString _) as v -> v | _ -> VPrim (PBool false))
         );
         ( "is_defined",
           ExtUnary (function VPrim PUndef -> VPrim (PBool false) | v -> v) );
         ( "is_prim",
           ExtUnary (function VPrim _ as v -> v | _ -> VPrim (PBool false)) );
         ( "length",
           ExtUnary
             (function
             | VPrim (PString s) -> VPrim (PInt (String.length s))
             | _ -> VPrim PUndef) );
         ( "has_field",
           ExtBinary
             (fun o f ->
               match (o, f) with
               | VObject vo, VPrim (PString vs) ->
                   VPrim (PBool (Object.mem vs vo))
               | _ -> VPrim PUndef) );
       ]
    |> Seq.map (fun (n, e) -> (n, VExtern e)))

let initial_state = { loc_cnt = 0; loc_map = LocMap.empty }

let prim_to_string = function
  | PInt i -> string_of_int i
  | PBool b -> string_of_bool b
  | PString s -> s
  | PUndef -> "undefined"

let string_of_value = function
  | VPrim p -> (
      match p with
      | PInt i -> string_of_int i
      | PBool b -> string_of_bool b
      | PString s -> "\"" ^ String.escaped s ^ "\""
      | PUndef -> "undefined")
  | VLoc _ -> "<location>"
  | VClosure _ -> "<closure>"
  | VObject _ -> "<object>"
  | VExtern _ -> "<extern>"

let string_of_result r =
  match r with
  | RValue v -> string_of_value v
  | RException e -> "Exception: " ^ string_of_value e

let string_of_env env =
  "{"
  ^ (Env.bindings env
    |> List.map (fun (k, v) -> k ^ ":" ^ string_of_value v)
    |> String.concat ",")
  ^ "}"

let string_of_state st =
  "{ loc_cnt = " ^ string_of_int st.loc_cnt ^ ", loc_map = {"
  ^ (LocMap.bindings st.loc_map
    |> List.map (fun (l, v) -> string_of_int l ^ ": " ^ string_of_value v)
    |> String.concat ",")
  ^ "}}"

let prim_of_value = function
  | VPrim p -> p
  | VLoc _ -> PUndef
  | VClosure _ -> PUndef
  | VObject _ -> PUndef
  | VExtern _ -> PUndef

let prim_of_opt = function Some v -> v | None -> PUndef

let int_of_prim = function
  | PInt i -> Some i
  | PBool true -> Some 1
  | PBool false -> Some 0
  | PString s -> int_of_string_opt s
  | PUndef -> None (* undefined *)

let int_of_value = function
  | VPrim p -> int_of_prim p
  | VLoc _ -> None
  | VClosure _ -> None
  | VObject _ -> None
  | VExtern _ -> None

let bool_of_prim = function
  | PInt i -> i <> 0
  | PBool b -> b
  | PString s -> s <> ""
  | PUndef -> false

let bool_of_value = function
  | VPrim p -> bool_of_prim p
  | VLoc _ -> true
  | VClosure _ -> true
  | VObject _ -> true
  | VExtern _ -> true

let new_ref v =
  ResultM.lift
    (StateM.state (fun st ->
         let cur = st.loc_cnt in
         (cur, { loc_cnt = cur + 1; loc_map = LocMap.add cur v st.loc_map })))

let eval_bop_plus v1 v2 =
  match (prim_of_value v1, prim_of_value v2) with
  | PString s1, p2 -> PString (s1 ^ prim_to_string p2)
  | p1, PString s2 -> PString (prim_to_string p1 ^ s2)
  | p1, p2 ->
      OptM.liftA2
        (fun i1 i2 -> PInt (i1 + i2))
        (int_of_prim p1) (int_of_prim p2)
      |> prim_of_opt

let eval_arith_bop f v1 v2 =
  VPrim (OptM.liftA2 f (int_of_value v1) (int_of_value v2) |> prim_of_opt)

let eval_div f v1 v2 =
  let open ResultM in
  match (int_of_value v1, int_of_value v2) with
  | Some _, Some 0 -> throw (VPrim (PString "Division by zero"))
  | Some i1, Some i2 -> pure (VPrim (PInt (f i1 i2)))
  | _ -> pure (VPrim PUndef)

let value_of_bool b = VPrim (PBool b)

let eval_compare_bop fs fi v1 v2 =
  match (prim_of_value v1, prim_of_value v2) with
  | PString s1, PString s2 -> fs s1 s2
  | p1, p2 -> (
      match (int_of_prim p1, int_of_prim p2) with
      | Some i1, Some i2 -> fi i1 i2
      | _ -> false)

let eval_compare_bop_v fs fi v1 v2 =
  value_of_bool (eval_compare_bop fs fi v1 v2)

let eval_eq_prim p1 p2 =
  match (p1, p2) with
  | PUndef, PUndef -> true
  | PBool b1, PBool b2 -> b1 = b2
  | PInt i1, PInt i2 -> i1 = i2
  | PString s1, PString s2 -> s1 = s2
  | (PInt _ | PBool _ | PString _), (PInt _ | PBool _ | PString _) ->
      OptM.liftA2 ( = ) (int_of_prim p1) (int_of_prim p2)
      |> Option.value ~default:false
  | _ -> false

let rec eval_eq v1 v2 =
  let open StateM in
  match (v1, v2) with
  | VPrim p1, VPrim p2 -> pure (eval_eq_prim p1 p2)
  | VLoc l1, VLoc l2 ->
      let* lm = gets (fun l -> l.loc_map) in
      eval_eq (LocMap.find l1 lm) (LocMap.find l2 lm)
  | VClosure _, VClosure _ -> pure false
  | VObject o1, VObject o2 ->
      eval_eq_obj (Object.bindings o1) (Object.bindings o2)
  | _ -> pure false

and eval_eq_obj o1 o2 =
  let open StateM in
  match (o1, o2) with
  | [], [] -> pure true
  | _, [] | [], _ -> pure false
  | (f1, _) :: _, (f2, _) :: _ when f1 <> f2 -> pure false
  | (_, v1) :: t1, (_, v2) :: t2 ->
      let* e = eval_eq v1 v2 in
      if e then eval_eq_obj t1 t2 else pure false

let rec eval_strict_eq v1 v2 =
  match (v1, v2) with
  | VPrim p1, VPrim p2 -> (
      match (p1, p2) with
      | PUndef, PUndef -> true
      | PUndef, _ -> false
      | PInt i1, PInt i2 -> i1 = i2
      | PInt _, _ -> false
      | PBool i1, PBool i2 -> i1 = i2
      | PBool _, _ -> false
      | PString s1, PString s2 -> s1 = s2
      | PString _, _ -> false)
  | VLoc l1, VLoc l2 -> l1 = l2
  | VLoc _, _ | _, VLoc _ -> false
  | VClosure _, _ | _, VClosure _ -> false
  | VExtern _, _ | _, VExtern _ -> false
  | VObject o1, VObject o2 ->
      eval_strict_eq_obj (Object.bindings o1) (Object.bindings o2)
  | VObject _, _ | _, VObject _ -> false

and eval_strict_eq_obj o1 o2 =
  match (o1, o2) with
  | [], [] -> true
  | _, [] | [], _ -> false
  | (f1, v1) :: t1, (f2, v2) :: t2 ->
      f1 = f2 && eval_strict_eq v1 v2 && eval_strict_eq_obj t1 t2

let eval_bop bop v1 v2 =
  let open ResultM in
  match bop with
  | BopPlus -> pure (VPrim (eval_bop_plus v1 v2))
  | BopMinus -> pure (eval_arith_bop (fun i1 i2 -> PInt (i1 - i2)) v1 v2)
  | BopTimes -> pure (eval_arith_bop (fun i1 i2 -> PInt (i1 * i2)) v1 v2)
  | BopDiv -> eval_div ( / ) v1 v2
  | BopMod -> eval_div ( mod ) v1 v2
  | BopLt ->
      pure
        (eval_compare_bop_v (fun s1 s2 -> s1 < s2) (fun i1 i2 -> i1 < i2) v1 v2)
  | BopLeq ->
      pure
        (eval_compare_bop_v
           (fun s1 s2 -> s1 <= s2)
           (fun i1 i2 -> i1 <= i2)
           v1 v2)
  | BopGt ->
      pure
        (eval_compare_bop_v (fun s1 s2 -> s1 > s2) (fun i1 i2 -> i1 > i2) v1 v2)
  | BopGeq ->
      pure
        (eval_compare_bop_v
           (fun s1 s2 -> s1 >= s2)
           (fun i1 i2 -> i1 >= i2)
           v1 v2)
  | BopEq -> value_of_bool <$> lift (eval_eq v1 v2)
  | BopNeq -> (fun b -> value_of_bool (not b)) <$> lift (eval_eq v1 v2)
  | BopEqStrict -> pure (value_of_bool (eval_strict_eq v1 v2))
  | BopNeqStrict -> pure (value_of_bool (not (eval_strict_eq v1 v2)))
  | BopAssign -> (
      match v1 with
      | VLoc l1 ->
          lift
            (StateM.state (fun l ->
                 (VPrim PUndef, { l with loc_map = LocMap.add l1 v2 l.loc_map })))
      | _ -> string_exn "Assignment to non-location")

let typeof_prim = function
  | PInt _ -> "int"
  | PBool _ -> "bool"
  | PString _ -> "string"
  | PUndef -> "undefined"

let typeof_value = function
  | VPrim p -> typeof_prim p
  | VLoc _ -> "location"
  | VClosure _ -> "closure"
  | VObject _ -> "object"
  | VExtern _ -> "closure"

let eval_uop uop v =
  let open StateM in
  match uop with
  | UopNot -> pure (value_of_bool (not (bool_of_value v)))
  | UopMinus ->
      pure
        (match int_of_value v with
        | Some i -> VPrim (PInt (-i))
        | None -> VPrim PUndef)
  | UopTypeof -> pure (VPrim (PString (typeof_value v)))
  | UopDeref -> (
      match v with
      | VLoc l -> gets (fun ls -> LocMap.find l ls.loc_map)
      | _ -> pure (VPrim PUndef))

module EvalM = ReaderT (ResultM)

let rec eval_expr_m e =
  let open EvalM in
  match e with
  | EBool b -> pure (VPrim (PBool b))
  | EInt i -> pure (VPrim (PInt (int_of_string i)))
  | ENegInt i -> pure (VPrim (PInt (int_of_string ("-" ^ i))))
  | EString s -> pure (VPrim (PString s))
  | EUndef -> pure (VPrim PUndef)
  | EBinOp (bop, e1, e2) ->
      let* v1 = eval_expr_m e1 in
      let* v2 = eval_expr_m e2 in
      lift (eval_bop bop v1 v2)
  | EUop (u, e) -> eval_expr_m e >>= fun v -> lift (ResultM.lift (eval_uop u v))
  | EAnd (e1, e2) ->
      let* v1 = eval_expr_m e1 in
      if bool_of_value v1 then
        map (fun v -> value_of_bool (bool_of_value v)) (eval_expr_m e2)
      else pure (value_of_bool false)
  | EOr (e1, e2) ->
      let* v1 = eval_expr_m e1 in
      if bool_of_value v1 then pure (value_of_bool true)
      else map (fun v -> value_of_bool (bool_of_value v)) (eval_expr_m e2)
  | EVar x -> (
      asks (Env.find_opt x) >>= function
      | Some v -> pure v
      | None -> lift (string_exn "Unbound variable"))
  | ELet (x, e1, e2) ->
      let* v = eval_expr_m e1 in
      local (Env.add x v) (eval_expr_m e2)
  | EFix (f, a, b) ->
      asks (fun env ->
          VClosure { cl_typ = ClFix f; cl_env = env; cl_args = a; cl_body = b })
  | EIf (e1, e2, e3) ->
      let* v = eval_expr_m e1 in
      if bool_of_value v then eval_expr_m e2 else eval_expr_m e3
  | ESeq (e1, e2) ->
      let* _ = eval_expr_m e1 in
      eval_expr_m e2
  | EWhile (c, b) -> eval_while c b
  | EFun (a, b) ->
      asks (fun env ->
          VClosure
            { cl_typ = ClRegular; cl_env = env; cl_args = a; cl_body = b })
  | EApp (f, ps) -> (
      let* vf = eval_expr_m f in
      match vf with
      | VClosure cl -> (
          let* a = eval_args cl.cl_env cl.cl_args ps in
          match cl.cl_typ with
          | ClRegular -> local (fun _ -> a) (eval_expr_m cl.cl_body)
          | ClFix f -> local (fun _ -> Env.add f vf a) (eval_expr_m cl.cl_body))
      | VExtern e -> (
          match (e, ps) with
          | ExtUnary ue, [ a0 ] -> ue <$> eval_expr_m a0
          | ExtBinary be, [ a0; a1 ] ->
              liftA2 be (eval_expr_m a0) (eval_expr_m a1)
          | _ -> lift (string_exn "Application: wrong number of arguments"))
      | _ -> lift (string_exn "Application: not a function"))
  | EThrow e -> eval_expr_m e >>= fun v -> lift (ResultM.throw v)
  | ETry (e1, x, e2) -> eval_try e1 x e2
  | ETryFinally (e1, x, e2, e3) ->
      fun env ->
        ResultM.catch_finally (eval_expr_m e1 env)
          (fun e -> eval_expr_m e2 (Env.add x e env))
          (eval_expr_m e3 env)
  | ERef e ->
      let* v = eval_expr_m e in
      (fun l -> VLoc l) <$> lift (new_ref v)
  | EObject fs -> eval_obj Object.empty fs
  | EGetField (oe, f) ->
      let* vo = eval_expr_m oe in
      let* vf = eval_field_name f in
      pure
        (match vo with
        | VObject o -> (
            match Object.find_opt vf o with Some r -> r | None -> VPrim PUndef)
        | _ -> VPrim PUndef)
  | EDeleteField (oe, f) ->
      let* vo = eval_expr_m oe in
      let* vf = eval_field_name f in
      pure (match vo with VObject o -> VObject (Object.remove vf o) | _ -> vo)
  | EUpdateField (o, f, e) ->
      let* vo = eval_expr_m o in
      let* vf = eval_field_name f in
      let* v = eval_expr_m e in
      pure (match vo with VObject o -> VObject (Object.add vf v o) | _ -> v)

and eval_try e1 x e2 env =
  ResultM.catch (eval_expr_m e1 env) (fun e -> eval_expr_m e2 (Env.add x e env))

and eval_while cond body =
  let open EvalM in
  let* c = eval_expr_m cond in
  if bool_of_value c then
    let* _ = eval_expr_m body in
    eval_while cond body
  else pure (VPrim PUndef)

and eval_args acc_env params args =
  let open EvalM in
  match (params, args) with
  | [], [] -> pure acc_env
  | _, [] | [], _ -> lift (string_exn "Application: wrong number of arguments")
  | hp :: tp, ha :: ta ->
      let* vh = eval_expr_m ha in
      eval_args (Env.add hp vh acc_env) tp ta

and eval_obj acc fields =
  let open EvalM in
  match fields with
  | [] -> pure (VObject acc)
  | (f, e) :: t ->
      let* l = eval_expr_m e in
      eval_obj (Object.add f l acc) t

and eval_field_name v =
  EvalM.map (fun v -> prim_to_string (prim_of_value v)) (eval_expr_m v)

let eval_expr (e, env, st) =
  let ms = ResultM.run_exn (EvalM.run_reader (eval_expr_m e) env) in
  StateM.run_state ms st

let eval_expr_init e = eval_expr (e, initial_env, initial_state)

let eval_defn (d, env, st) =
  match d with
  | DLet (x, e) -> (
      let r, s1 = eval_expr (e, env, st) in
      match r with
      | RValue v -> (r, Env.add x v env, s1)
      | RException _ -> (r, env, s1))

let eval_phrase (p, env, st) =
  match p with
  | Expr e ->
      let r, s1 = eval_expr (e, env, st) in
      (r, env, s1)
  | Defn d -> eval_defn (d, env, st)
