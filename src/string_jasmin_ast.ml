open Jasmin_ast

let string_ptype x : string = 
match x with 
| TBool -> "bool"
| TInt -> "int"
| T_utype y -> match y with 
                | T_u64 -> "u64"

let string_jasmin_storage x : string = 
match x with
| Reg p -> "reg " ^ string_ptype p
| Stack p -> "stack " ^ string_ptype p
| Inline p -> "inline " ^ string_ptype p

let string_jasmin_peop2 x : string =
match x with 
| Pequality -> " == "
| Pand -> " && "
| PBitwiseAnd -> " & "
| PBitwiseOr -> " | "
| PInequality -> " != "
| Pleq -> " <= "
| Padd -> " + "

let string_jasmin_peop1 x : string =
match x with 
| Pnot -> "!"
| PBitwisenot -> "-"

let rec string_expr x : string = 
match x with
| Pvar n -> n 
| Pbool b -> if b then "true" else "false"
| Pconst i -> string_of_int i
| Pbinop (op, e1, e2)-> "(" ^ (string_expr e1) ^ (string_jasmin_peop2 op) ^ (string_expr e2) ^ ")"
| Punop (op, e) -> "(" ^ (string_jasmin_peop1 op) ^ (string_expr e) ^ ")"
| _ -> " "

let rec string_jasmin_instr x = 
  match x with 
  | PVarDecl (s, vname) -> (string_jasmin_storage s) ^ " " ^ vname ^ ";"
  | PAssgn (e1,e2) -> (string_expr e1) ^ " = " ^ (string_expr e2) ^ ";"
  | Pcmov (e1,e2,e3) -> (string_expr e1) ^ " = " ^ (string_expr e2) ^ " if " ^ (string_expr e3) ^ ";"
  | PWhile (e,s) -> "while " ^ (string_expr e) ^ " {\n" ^ 
  (List.fold_left (fun acc x -> acc ^ (string_jasmin_instr x) ^ "\n") "" s) ^ "}"


let string_jasmin_func f : string = 
  let fn_def = ("export fn " ^ f.f_name ^ " (") ^ 
  (List.fold_left (fun acc (arg, ty) -> acc ^ 
  ((string_jasmin_storage ty) ^ " " ^ string_expr arg ^ ", " )) "" 
  (List.combine f.f_args f.f_type_in)) in

  let f_body = List.fold_left (fun acc x -> acc ^ (string_jasmin_instr x) ^ "\n") "" f.f_body in 

  let f_ret = ("return " ^ (string_expr f.f_ret) ^ ";\n}") in 
  
  (String.sub fn_def 0 ((String.length fn_def) - 2)) ^
  (") -> " ^ (string_jasmin_storage f.f_type_out) ^ " {\n") ^ f_body ^ f_ret
