open Jasmin_ast
open Util
open Pos
open Err
open Tast 
open Pseudocode 

class vardec_collector m =
  object (visit)
    inherit Tastmap.tast_visitor m as super
    val mutable _vars : (var_name * base_type) list = []

    method _vars () =
      List.rev _vars

    method block_only (block,next) =
      begin
        match block.data with
          | RangeFor (x,bty,_,_,_)
          | ArrayFor (x,bty,_,_) ->
            _vars <- (x,bty) :: _vars
          | _ -> ()
      end; super#block_only (block,next)

    method stm stm =
      begin
        match stm.data with
          | VarDec (x,bty,_)
          | FnCall (x,bty,_,_) ->
            _vars <- (x,bty) :: _vars
          | _ -> ()
      end; super#stm stm
  end

let collect_vardecs fdec =
  let m = Module([],[fdec],{fmap=[]}) in
  let visit = new vardec_collector m in
    visit#fact_module () |> ignore;
    visit#_vars ()

class codegen m = 
  
  object (visit)

    method bty {pos=p;data} : jasmin_ptype =
      match data with
        | Bool _ -> T_utype (T_u64)
        | UInt (s,_) | Int (s,_) -> T_utype (T_u64)
        | Ref (bty,_) -> visit#bty bty
        | _ -> print_endline "We ran into an error" ; TInt

    method fact_module () : string =
      let Module (sdecs,fdecs,minfo) = m in
        let f = List.map visit#fdec (List.rev fdecs) in

        List.fold_left (fun acc x -> acc ^ (String_jasmin_ast.string_jasmin_func x) ^ "\n" ) "" f 
        (* List.map (fun x -> (Pp_jasmin.pp_jasmin_func x)) f *)
    
    method retValue {pos=p;data} : jasmin_pexpr = 
        match data with
        | Block ({pos=p;data}, next) -> visit#retValue next
        | Return e -> visit#expr e
    
    method fdec ({pos=p;data} as fdec) =
      match data with
      | FunDec (name,fnattr,rt,params,body) ->

          let f_type_in = List.map (fun param -> match param.data with
          | Param (x,bty) -> Reg (visit#bty bty)) params in 

          let f_arg = List.map (fun param -> match param.data with
          | Param (x,bty) -> Pvar(x.data)) params in

          let f_type_out = match rt with 
          | Some bty -> (Reg (visit#bty bty)) in 

          let var_decs = 
          [PVarDecl (Reg (T_utype T_u64), "t"); PAssgn(Pvar "t", Pconst(1));
          PVarDecl (Reg (T_utype T_u64), "f"); PAssgn(Pvar "f", Pconst(0)); PVarDecl(Reg (T_utype T_u64), "intermediate")
          ; PVarDecl(Reg (TBool), "intermediateBool")] @

          List.map (fun (x,bty) -> 
            PVarDecl (Reg (visit#bty bty), x.data))
          (collect_vardecs fdec)  in 
          
          let retValueStart = match body with 
          | ({pos=p;data}, next) -> next in 

          { f_name = name.data; f_type_in = f_type_in;
                f_args = f_arg; 
                f_body = var_decs @ (visit#block body); 
                f_type_out = f_type_out;
                f_ret =  visit#retValue retValueStart } 

    method param {pos=p;data} =
      match data with
        | Param (x,bty) ->
          visit#bty bty

    method block ({pos=p;data},next) : jasmin_stmt =
      match data with
      | Scope blk -> visit#block blk @ visit#next next
      | ListOfStuff stms -> (List.fold_left (fun (acc: jasmin_stmt) x -> acc @ (visit#stm x)) [] stms)
      @ visit#next next
      | RangeFor (x,bty,e1,e2,blk) ->
          [PAssgn(Pvar x.data, visit#expr e1)]
          @ [PWhile(Pbinop(Pleq, Pvar x.data, visit#expr e2), visit#block blk @ [PAssgn(Pvar x.data, Pbinop(Padd, (Pvar x.data), Pconst(1)))])] @ visit#next next

    method next {pos=p;data} : jasmin_stmt =
      match data with
        | Block blk ->
            visit#block blk
        | Return e -> []
        | End -> []

    method stm {pos=p;data} : jasmin_stmt =

     let process_bool_code ({pos=p;data},bty) name = 
          match data with 
          | BinOp (op,_,_) -> 
            begin 
            match op with 
            | Ast.Equal | Ast.LogicalAnd | Ast.BitwiseAnd -> let v = visit#expr ({pos=p;data},bty) in
            [PAssgn(Pvar "intermediateBool", v); PAssgn(name, (Pvar "f")); Pcmov (name, (Pvar "t"), (Pvar "intermediateBool"))]
            | _ -> [PAssgn(name, visit#expr ({pos=p;data},bty))]
            end
          | UnOp (op, e) -> 
            begin 
            match op with 
            | Ast.LogicalNot -> let v = visit#expr ({pos=p;data},bty) in
            [PAssgn(Pvar "intermediateBool", v); PAssgn(name, (Pvar "f")); Pcmov (name, (Pvar "t"), (Pvar "intermediateBool"))]
            | _ -> [PAssgn(name, visit#expr ({pos=p;data},bty))]
            end 
          | _ -> [PAssgn(name, visit#expr ({pos=p;data},bty))] in
      
      match data with
      | VarDec (name,bty,e2) -> 
        (* I don't this this would work if we made three comparisions *)
        (* x = y && z && p ? *)
        (* maybe we should flatten the binop to deal with this? *)
        (process_bool_code e2 (Pvar name.data)) 
      | Assign (e1,e2) -> 
        [PAssgn(visit#expr e1, visit#expr e2)] 
      | Cmov (e1,cond,e2) ->

          (* when we compile expressions that evaluate to booleans we always want to expand them out such that on each 
          line only two computations happen at once *)
          (* for now let's just assume for it the two case comparison *)
          let findOp op =
            match op with
            | Ast.Equal -> Pequality
            | Ast.LogicalAnd -> PBitwiseAnd
            | Ast.LogicalOr -> PBitwiseOr
          in 

          let rec flattenBinOp ({pos=p;data},bty)  = 
            match data with           
            | BinOp (op,e1,e2) -> 
                flattenBinOp e1 @
                [PAssgn(Pvar "intermediate", Pbinop(findOp op, Pvar "intermediate", visit #expr e2))]
                | _ -> [PAssgn(Pvar "intermediate", visit#expr ({pos=p;data},bty))] 
                in 

          let lle1 = visit#expr e1 in
          let lle2 = visit#expr e2 in
          let llcond = visit#expr cond in

          flattenBinOp cond @ 
          [Pcmov (lle1, lle2, Pbinop(Pequality, Pvar "t", Pvar "intermediate"))]

    method expr ({pos=p;data},bty) : jasmin_pexpr =
      let llbty = visit#bty bty in
      match data with
      | True -> (Pvar "t")
      | False -> (Pvar "f")
      | BinOp (op,e1,e2) -> 
        let lle1 = visit#expr e1 in
        let lle2 = visit#expr e2 in
        visit#binop op lle1 lle2
      | UnOp (op, e) ->
        let lle = visit#expr e in
        (Pbinop (PInequality, Pvar "t", lle))
      | IntLiteral n -> Pconst (n)
      | Variable x -> (Pvar x.data) 
      | Classify e -> visit#expr e
      | Enref e -> visit#expr e 
      | Deref e -> visit#expr e 
      | _ -> raise @@ cerr p "unimplemented in codegen: %s" (show_expr' data) 
    
    method binop op lle1 lle2 =
      match op with
      | Ast.Equal -> Pbinop (Pequality, lle1, lle2) 
      | Ast.LogicalAnd -> Pbinop (PBitwiseAnd, lle1, lle2) 
      | Ast.LogicalOr -> Pbinop (PBitwiseOr, lle1, lle2) 
      
    method unop op lle =
      match op with
      | Ast.LogicalNot -> Punop (Pnot, lle)

    end 

let codegen m : string =
  let visit = new codegen m in
      visit#fact_module ()

let codegen_ec m file_name : string =
    let Module (sdecs,fdecs,minfo) = m in
    let extract_public_params ({pos=p;data} as fdec) =
        match data with
        | FunDec (name,fnattr,rt,params,body) -> 
        (name.data, List.fold_left (fun acc param -> match param.data with
                                        | Param (x,{pos=p;data}) -> 
                                        begin
                                            match data with 
                                            | Bool (l) -> 
                                                begin 
                                                match l.data with 
                                                | Tast.Public -> x.data :: acc
                                                | _ -> [] 
                                                end  
                                            | UInt (s,l) | Int (s,l) -> 
                                                begin 
                                                match l.data with 
                                                | Tast.Public -> x.data :: acc
                                                | _ -> [] 
                                                end 
                                        end 
                        ) [] params) in 

    (* We need to edit this such that we output a good ec file *)
    List.fold_left (fun acc x -> 
        let f_name, lst = extract_public_params x in 
        acc ^ (Ec_file.output lst (String.sub file_name 2 ((String.length file_name)-2)) f_name)) "" (List.rev fdecs) 
