
exception TransformError of string

type context = Context of Cast.expr

let ctx_expr (Context(e)) = e

let new_temp_var =
  let cntr = ref 0 in
  let new_temp_var'() =
    cntr := !cntr + 1;
    "m" ^ (string_of_int !cntr)
  in
  new_temp_var'

let b_true = Cast.Primitive(Cast.Number (-1))
let b_false = Cast.Primitive(Cast.Number 0)
let b_and l r = Cast.BinOp(Cast.BitAnd,l,r)
let b_or l r = Cast.BinOp(Cast.BitOr,l,r)
let b_not e = Cast.UnOp(Cast.BitNot,e)

let rec transform = function
  | Ast.CModule fdecs ->
    let f = List.map transform_fdec fdecs in
    Cast.CModule f

and transform_type = function
  | Ast.Int -> Cast.Int
  | Ast.Bool -> Cast.Int
  | Ast.ByteArr s -> Cast.ByteArr s

and transform_arg {Ast.name=n; Ast.ty=t} =
  {Cast.name=n; Cast.ty=transform_type(t)}

and transform_stm ctx = function
  | Ast.VarDec(n,ty,v) ->
    let ty' = transform_type(ty) in
    let v' = transform_expr(v) in
    [Cast.VarDec(n,ty',v')]
  | Ast.Assign(n,v) ->
    let c = ctx_expr ctx in
    let v' = transform_expr(v) in
    let assign_ok = b_and c (b_not (Cast.VarExp "rset")) in
    let newval = b_and v' assign_ok in
    let oldval = b_and (Cast.VarExp n) (b_not assign_ok) in
    [Cast.Assign(n,(b_or newval oldval))]
  | Ast.ArrAssign(n,i,v) ->
    let c = ctx_expr ctx in
    let v' = transform_expr(v) in
    let assign_ok = b_and c (b_not (Cast.VarExp "rset")) in
    let newval = b_and v' assign_ok in
    let oldval = b_and (Cast.ArrExp(n,i)) (b_not assign_ok) in
    [Cast.ArrAssign(n,i,(b_or newval oldval))]
  | Ast.If(e,bt,bf) ->
    let c = ctx_expr ctx in
    let e' = transform_expr(e) in
    let tname = new_temp_var() in
    let m = Cast.VarExp tname in
    let c' = b_and m c in
    let ctx' = Context(c') in
    let bt' = List.flatten(List.map (transform_stm ctx') bt) in
    let bf' = List.flatten(List.map (transform_stm ctx') bf) in
    let mdec = Cast.VarDec(tname,Cast.Int,b_and e' c) in
    let mnot = Cast.Assign(tname,b_not m) in
    [mdec] @ bt' @ [mnot] @ bf'
  | Ast.For(n,l,h,b) ->
    let l' = transform_primitive l in
    let h' = transform_primitive h in
    let b' = List.flatten(List.map (transform_stm ctx) b) in
    [Cast.For(n,l',h',b')]
  | Ast.Return e ->
    let c = ctx_expr ctx in
    let e' = transform_expr(e) in
    let rval = Cast.VarExp "rval" in
    let rset = Cast.VarExp "rset" in
    let assign_ok = b_and c (b_not rset) in
    let newval = b_and e' assign_ok in
    [Cast.Assign("rval",(b_or rval newval)); Cast.Assign("rset",(b_or rset c))]

and transform_expr = function
  | Ast.VarExp s -> Cast.VarExp s
  | Ast.ArrExp(s,i) -> Cast.ArrExp(s,i)
  | Ast.Unop(u,e) -> Cast.UnOp(transform_unop(u),transform_expr(e))
  | Ast.BinOp(b,e1,e2) ->
    let b' = transform_binop b in
    let e1' = transform_expr e1 in
    let e2' = transform_expr e2 in
    Cast.BinOp(b',e1',e2')
  | Ast.Primitive p -> Cast.Primitive(transform_primitive p)
  | Ast.CallExp(n,args) ->
    let args' = List.map transform_expr args in
    Cast.CallExp(n,args')

and transform_primitive = function
  | Ast.Number n -> Cast.Number n
  | Ast.Boolean true -> Cast.Number (-1)
  | Ast.Boolean false -> Cast.Number 0
  | Ast.ByteArray s -> Cast.ByteArray s

and transform_unop = function
  | Ast.B_Not -> Cast.BitNot

and transform_binop = function
  | Ast.Plus -> Cast.Plus
  | Ast.Minus -> Cast.Minus
  | Ast.GT -> Cast.GT
  | Ast.B_And -> Cast.BitAnd
  | Ast.B_Or -> Cast.BitOr

and transform_fdec = function
  | Ast.FunctionDec(name,args,rt,body) ->
    let args' = List.map transform_arg args in
    let rt' = transform_type(rt) in
    let ctx = Context(Cast.Primitive(Cast.Number (-1))) in
    let body' = List.flatten(List.map (transform_stm ctx) body) in
    let rval = Cast.VarDec("rval",Cast.Int,Cast.Primitive(Cast.Number 0)) in
    let rset = Cast.VarDec("rset",Cast.Int,Cast.Primitive(Cast.Number 0)) in
    let body'' = [rval]@[rset]@body' in
    Cast.FunctionDec(name,args',rt',body'',Cast.VarExp("rval"))