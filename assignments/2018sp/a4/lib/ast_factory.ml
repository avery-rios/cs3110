open Ast

let make_let_defn x e = DLet (x, e)
let make_let_rec_defn f x e = DLet (f, EFix (f, x, e))
let make_seq e1 e2 = ESeq (e1, e2)
let make_app e es = EApp (e, es)

let make_unop uop e =
  match (uop, e) with UopMinus, EInt i -> ENegInt i | _, _ -> EUop (uop, e)

let make_binop bop e1 e2 = EBinOp (bop, e1, e2)
let make_and e1 e2 = EAnd (e1, e2)
let make_or e1 e2 = EOr (e1, e2)
let make_if e1 e2 e3 = EIf (e1, e2, e3)
let make_if_partial e1 e2 = EIf (e1, e2, EUndef)
let make_let x e1 e2 = ELet (x, e1, e2)
let make_let_rec f x e1 e2 = ELet (f, EFix (f, x, e1), e2)
let make_try e1 x e2 = ETry (e1, x, e2)
let make_try_finally e1 x e2 e3 = ETryFinally (e1, x, e2, e3)
let make_throw e = EThrow e
let make_ref e = ERef e
let make_fun xs e = EFun (xs, e)
let make_while e1 e2 = EWhile (e1, e2)
let make_delete_field e1 e2 = EDeleteField (e1, e2)
let make_var x = EVar x
let make_int s = EInt s
let make_string s = EString s
let make_bool b = EBool b
let make_undefined () = EUndef
let make_object fields = EObject fields
let make_get_field e1 e2 = EGetField (e1, e2)
let make_update_field e f e2 = EUpdateField (e, f, e2)
