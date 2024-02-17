open Ast

(******************************************************************************
   This AST "factory" contains functions that the parser calls to produce
   AST nodes.  This design enables the parser to be ignorant of the
   names of the constructors of your AST, thus enabling everyone in the
   class to design their own AST type.

   You don't want to change any of the names or types appearing in this
   factory, because then the Menhir parser in `parser.mly` would have to be
   modified---something you really don't want to do.
 ******************************************************************************)

val make_let_defn : id -> expr -> defn
(** [make_let_defn x e] represents [let x = e] *)

val make_let_rec_defn : id -> id list -> expr -> defn
(** [make_let_rec_defn f xs e] represents [let rec f xs = e] *)

val make_seq : expr -> expr -> expr
(** [make_seq e1 e2] represents [e1; e2] *)

val make_app : expr -> expr list -> expr
(** [make_app e0 [e1; e2; ...; en]] represents [e0 e1 e2 ... en] *)

val make_unop : unop -> expr -> expr
(** [make_unop u e] represents [u e] *)

val make_binop : binop -> expr -> expr -> expr
(** [make_binop b e1 e2] represents [e1 b e2] *)

val make_and : expr -> expr -> expr
(** [make_and e1 e2] represents [e1 && e2] *)

val make_or : expr -> expr -> expr
(** [make_or e1 e2] represents [e1 || e2] *)

val make_if : expr -> expr -> expr -> expr
(** [make_if e1 e2 e3] represents [if e1 then e2 else e3] *)

val make_if_partial : expr -> expr -> expr
(** [make_if_partial e1 e2] represents [if e1 then e2] *)

val make_let : id -> expr -> expr -> expr
(** [make_let x e1 e2] represents [let x = e1 in e2] *)

val make_let_rec : id -> id list -> expr -> expr -> expr
(** [make_let_rec f xs e1 e2] represents [let rec f xs = e1 in e2] *)

val make_try : expr -> id -> expr -> expr
(** [make_try e1 x e2] represents [try e1 catch x handle e2] *)

val make_try_finally : expr -> id -> expr -> expr -> expr
(** [make_try_finally e1 x e2 e3] represents
   [try e1 catch x handle e2 finally e3] *)

val make_throw : expr -> expr
(** [make_throw e] represents [throw e] *)

val make_ref : expr -> expr
(** [make_ref e] represents [ref e] *)

val make_fun : id list -> expr -> expr
(** [make_fun [x1; ...; xn] e] represents [fun (x1 ... xn) e] *)

val make_while : expr -> expr -> expr
(** [make_while e1 e2] represents [while e1 do e2 done] *)

val make_delete_field : expr -> expr -> expr
(** [make_delete_field e1 e2] represents [delete e1[e2]] *)

val make_var : id -> expr
(** [make_var x] represents [x] *)

val make_int : string -> expr
(** [make_int s] represents an integer whose string representation is [s].
   The type is not typo:  the parser returns a string, not an int. *)

val make_string : string -> expr
(** [make_string s] represents the string [s] *)

val make_bool : bool -> expr
(** [make_bool b] represents the boolean [b] *)

val make_undefined : unit -> expr
(** [make_undefined ()] represents [undefined] *)

val make_object : (id * expr) list -> expr
(** [make_object [x1, e1; ...; xn, en]] represents [{x1:e1, ..., xn:en}]. *)

val make_get_field : expr -> expr -> expr
(** [make_get_field e1 e2] represents [e1[e2]] *)

val make_update_field : expr -> expr -> expr -> expr
