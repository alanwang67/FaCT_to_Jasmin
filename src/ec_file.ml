

let output lst file_name func_name = 
  let public_params = List.fold_left (fun acc x -> acc ^ x ^ ", ") "" lst in
  let ec_leakage_file = (String.capitalize_ascii file_name) ^ "_ct" in 
  "require " ^ ec_leakage_file ^ ".\n\n" ^
  "import var " ^ ec_leakage_file ^ ".M.\n\n" ^ 
  "equiv " ^ ec_leakage_file ^ " :\n" ^ 
  "  " ^ ec_leakage_file ^ ".M." ^ func_name ^ " ~ " ^ ec_leakage_file ^ ".M." ^func_name ^ " :\n" ^ 
  "  " ^ "={leakages, " ^ (String.sub (public_params) 0 ((String.length public_params) - 2)) ^"} ==> ={leakages}.\n" ^
  "  proof. proc; inline *; sim. qed."
