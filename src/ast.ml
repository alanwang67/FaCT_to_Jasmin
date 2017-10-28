open Pos

type size = int [@@deriving show]

type var_name' = string [@@deriving show]
and var_name = var_name' pos_ast [@@deriving show]

type fun_name' = string [@@deriving show]
and fun_name = fun_name' pos_ast [@@deriving show]

type mutability' =
  | Const
  | Mut
[@@deriving show]
and mutability = mutability' pos_ast [@@deriving show]

and label' =
  | Public
  | Secret
  | Unknown
[@@deriving show]
and label = label' pos_ast [@@deriving show]

and base_type' =
  | UInt of size
  | Int of size
  | Bool
[@@deriving show]
and base_type = base_type' pos_ast [@@deriving show]

and lexpr' =
  | LIntLiteral of int
  | LUnspecified
[@@deriving show]
and lexpr = lexpr' pos_ast [@@deriving show]

and array_type' =
  | ArrayAT of base_type * lexpr
[@@deriving show]
and array_type = array_type' pos_ast [@@deriving show]

and expr_type' =
  | BaseET of base_type * label
  | ArrayET of array_type * label * mutability
[@@deriving show]
and expr_type = expr_type' pos_ast [@@deriving show]

and variable_type' =
  | RefVT of base_type * label * mutability
  | ArrayVT of array_type * label * mutability
[@@deriving show]
and variable_type = variable_type' pos_ast [@@deriving show]

and expr' =
  | True
  | False
  | IntLiteral of int
  | Variable of var_name
  | ArrayGet of var_name * expr
  | ArrayLen of var_name
  | IntCast of base_type * expr
  | UnOp of unop * expr
  | BinOp of binop * expr * expr
  | TernOp of expr * expr * expr
  | FnCall of fun_name * arg_exprs
  | Declassify of expr
[@@deriving show]
and expr = expr' pos_ast [@@deriving show]

and unop =
  | Neg
  | LogicalNot
  | BitwiseNot
[@@deriving show]

and binop =
  | Plus
  | Minus
  | Multiply
  | Equal
  | NEqual
  | GT
  | GTE
  | LT
  | LTE
  | LogicalAnd
  | LogicalOr
  | BitwiseAnd
  | BitwiseOr
  | BitwiseXor
  | LeftShift
  | RightShift
[@@deriving show]

and array_expr' =
  | ArrayLit of expr list
  | ArrayVar of var_name
  | ArrayZeros of lexpr
  | ArrayCopy of var_name
  | ArrayView of var_name * expr * lexpr
  | ArrayComp of base_type * lexpr * var_name * expr
[@@deriving show]
and array_expr = array_expr' pos_ast [@@deriving show]

and arg_exprs = arg_expr list [@@deriving show]

and arg_expr' =
  | ByValue of expr
  | ByArray of array_expr * mutability
  | ByRef of var_name
[@@deriving show]
and arg_expr = arg_expr' pos_ast [@@deriving show]

and array_index = expr [@@deriving show]
and cond = expr [@@deriving show]
and thenstms = statements [@@deriving show]
and elsestms = statements [@@deriving show]
and statements = statement list [@@deriving show]
and low_expr = expr [@@deriving show]
and high_expr = expr [@@deriving show]

and statement' =
  | BaseDec of var_name * variable_type * expr
  | ArrayDec of var_name * variable_type * array_expr
  | BaseAssign of var_name * expr
  | ArrayAssign of var_name * array_index * expr
  | If of cond * thenstms * elsestms
  | For of var_name * base_type * low_expr * high_expr * statements
  | VoidFnCall of fun_name * arg_exprs
  | Return of expr
  | VoidReturn
[@@deriving show]
and statement = statement' pos_ast [@@deriving show]

and param' =
  | Param of var_name * variable_type
[@@deriving show]
and param = param' pos_ast [@@deriving show]

and params = param list [@@deriving show]
and body = statements [@@deriving show]

and ret_type = expr_type option [@@deriving show]

and function_dec' =
  | FunDec of fun_name * ret_type * params * body
  | CExtern of fun_name * ret_type * params
[@@deriving show]
and function_dec = function_dec' pos_ast [@@deriving show]

and function_decs = function_dec list
[@@deriving show]

and fact_module =
  | Module of function_decs
[@@deriving show]

(* Used to parse a top level value in the REPL *)
and top_level =
| FunctionDec of function_dec
| Statement of statement
| Expression of expr
[@@deriving show]
