open Pos
open Err

let pp_hashtbl fmt vtbl =
  let pp = Format.pp_print_text fmt in
    begin
      pp "{ ";
      Hashtbl.iter
        (fun k v -> pp (k ^ "; "))
        vtbl;
      pp "}";
    end

type 'a envtbl = (string,'a) Hashtbl.t [@printer pp_hashtbl]

type 'a env =
  | TopEnv of 'a envtbl
  | SubEnv of 'a envtbl * 'a env

let pp_env fmt env =
  let pp = Format.pp_print_text fmt in
    begin
      pp "{ ";
      (match env with
        | TopEnv vtbl -> pp_hashtbl fmt vtbl
        | SubEnv(vtbl,_) -> pp_hashtbl fmt vtbl);
      pp "}";
    end

let new_env () = TopEnv (Hashtbl.create 10)

let sub_env env = SubEnv (Hashtbl.create 10, env)

let get_vtbl = function
  | TopEnv vtbl -> vtbl
  | SubEnv(vtbl,_) -> vtbl

let add_var env v lt =
  let vtbl = get_vtbl env in
    if Hashtbl.mem vtbl v.data then raise (errRedefVar v);
    Hashtbl.add vtbl v.data lt

let rec find_var env =
  let find_var' fn vtbl v =
    try
      Hashtbl.find vtbl v.data
    with
        Not_found -> fn v
  in
    match env with
      | TopEnv vtbl ->
        find_var' (fun v -> raise @@ errVarNotDefined v) vtbl
      | SubEnv(vtbl,env') ->
        find_var' (find_var env') vtbl