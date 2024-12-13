open Tast

type jasmin_utype = 
| T_u64 [@@deriving show]

type jasmin_ptype = 
| TBool
| TInt
| T_utype of (jasmin_utype) [@@deriving show]

type jasmin_peop1 =
| PBitwisenot
| Pnot [@@deriving show]

type jasmin_peop2 =
| Padd 
| Psubtract
| Pmultiply 
| Pand
| PBitwiseAnd
| PBitwiseOr
| Pleq
| PInequality
| Pequality [@@deriving show]

type jasmin_pexpr =
| Pconst of int
| Pbool  of bool
| Pvar   of string
| Pbinop of jasmin_peop2 * jasmin_pexpr * jasmin_pexpr
| Punop of jasmin_peop1 * jasmin_pexpr
[@@deriving show]

type jasmin_cc = 
| Export 
| Inline [@@deriving show]

type jasmin_storage =
| Reg of jasmin_ptype
| Stack of jasmin_ptype
| Inline of jasmin_ptype [@@deriving show]

type jasmin_instr = 
| PAssgn of jasmin_pexpr * jasmin_pexpr 
| PVarDecl of jasmin_storage * string
| Pcmov of jasmin_pexpr * jasmin_pexpr * jasmin_pexpr 
| PWhile of jasmin_pexpr * jasmin_stmt
and jasmin_stmt = jasmin_instr list [@@deriving show]

type jasmin_func = {
  f_name : string; 
  f_type_in : jasmin_storage list;
  f_args : jasmin_pexpr list; 
  f_body : jasmin_stmt;
  f_type_out : jasmin_storage;
  f_ret : jasmin_pexpr; 
} [@@deriving show]

type jasmin_gdec = 
  | MIfun of jasmin_func
  | MIparam of jasmin_instr
  | MIglobal of jasmin_pexpr * jasmin_pexpr [@@deriving show]

type jasmin_prog = jasmin_gdec list [@@deriving show]


