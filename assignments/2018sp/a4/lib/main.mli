(* [interp s] interprets [s] as an expression.  This is useful for testing
   in a test suite or in the toplevel *)
val interp_expr : string -> string

(* [inter_phrase (s, env, st)] interprets [s] in an environment and state,
   yielding a new environment and state.  The REPL uses this to
   interpret input from the user. *)
val interp_phrase : string * Eval.env * Eval.state
                    -> string * Eval.env * Eval.state
