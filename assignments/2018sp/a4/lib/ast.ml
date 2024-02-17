(******************************************************************************
   These types (id, unop, binop) are used by the parser.  You do not want to
   change them.
 ******************************************************************************)

type id = string
type unop = UopMinus | UopNot | UopTypeof | UopDeref

type binop =
  | BopPlus
  | BopMinus
  | BopTimes
  | BopDiv
  | BopMod
  | BopLt
  | BopLeq
  | BopGt
  | BopGeq
  | BopEq
  | BopNeq
  | BopEqStrict
  | BopNeqStrict
  | BopAssign

(******************************************************************************
   [expr] is the type of the AST for expressions. You may implement
   this type however you wish.  Use the example interpreters seen in
   lecture and lab as inspiration.
 ******************************************************************************)
type expr =
  | EInt of string
  | ENegInt of string
  | EString of string
  | EBool of bool
  | EUndef
  | EBinOp of binop * expr * expr
  | EUop of unop * expr
  | EAnd of expr * expr
  | EOr of expr * expr
  | EVar of string
  | ELet of string * expr * expr
  | EFix of string * id list * expr
  | EIf of expr * expr * expr
  | ESeq of expr * expr
  | EWhile of expr * expr
  | EFun of id list * expr
  | EApp of expr * expr list
  | EThrow of expr
  | ETry of expr * string * expr
  | ETryFinally of expr * string * expr * expr
  | ERef of expr
  | EObject of (id * expr) list
  | EGetField of expr * expr
  | EDeleteField of expr * expr
  | EUpdateField of expr * expr * expr

(******************************************************************************
   [defn] is the type of the AST for definitions. You may implement
   this type however you wish.  There is only one kind of
   definition---the let [rec] definition---so this type can be quite
   simple.
 ******************************************************************************)
type defn = DLet of id * expr

(******************************************************************************
   [pharse] is teh type of the AST for phrases. It is used by the
   parser.  You do not want to change it.
 ******************************************************************************)
type phrase = Expr of expr | Defn of defn
