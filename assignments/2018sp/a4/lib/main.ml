(******************************************************************************
   You do not need to modify anything in this file.
 ******************************************************************************)

open Eval

let interp_expr s =
  try
    s |> Parse.parse_expr |> Eval.eval_expr_init |> fun (r, _) ->
    string_of_result r
  with Parse.SyntaxError s | Failure s -> s

let interp_phrase (s, env, st) =
  try
    (s |> Parse.parse_phrase |> fun p -> Eval.eval_phrase (p, env, st))
    |> fun (r, env', st') -> (string_of_result r, env', st')
  with
  | Parse.SyntaxError s | Failure s -> (s, env, st)
  | End_of_file -> ("", env, st)
