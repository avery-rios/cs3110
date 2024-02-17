exception SyntaxError of string

(* [parse_expr s] parses [s] as an expression *)
val parse_expr : string -> Ast.expr

(* [parse_phrase s] parses [s] as a phrase *)
val parse_phrase : string -> Ast.phrase
