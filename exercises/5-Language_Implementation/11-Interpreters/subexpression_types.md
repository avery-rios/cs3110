Not all subexpression is well typed in context `ctx`. 
`let x = 0 <= 1 in if x then 1 else 0` is well typed in context `{x:Int}`,
but subexpression `if x then 1 else 0` is not.

Yes, if a subexpression is ill typed in all context, then the expression
would be ill typed too.