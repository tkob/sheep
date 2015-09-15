(* Code Generator for Tcl Assembly Language *)
structure GenTal = struct
  open Parse.Ast

  datatype opcode = PushInt of int
                  | PushStr of string
                  | Pop
                  | Load of string
                  | Store of string
                  | List of int
                  | Eq
                  | Gt
                  | Lt
                  | Ge
                  | Le
                  | Add
                  | Sub
                  | Mult
                  | Div
                  | InvokeStk of int
                  | ListIndexImm of int
                  | ListLength
                  | Nsupvar of string
                  | Label of string
                  | Jump of string
                  | JumpTrue of string
                  | JumpFalse of string
                  | LoadArray of string

  datatype context = SV | MV (* multi-value context *)

  fun puts s = (print s; print "\n")

  fun emit (PushInt value)      = puts ("push " ^ Int.toString value)
    | emit (PushStr value)      = puts ("push {" ^ value ^ "}")
    | emit (Pop)                = puts ("pop")
    | emit (Load varname)       = puts ("load " ^ varname)
    | emit (Store varname)      = puts ("store " ^ varname)
    | emit (List len)           = puts ("list " ^ Int.toString len)
    | emit (Eq)                 = puts ("eq")
    | emit (Gt)                 = puts ("gt")
    | emit (Lt)                 = puts ("lt")
    | emit (Ge)                 = puts ("ge")
    | emit (Le)                 = puts ("le")
    | emit (Add)                = puts ("add")
    | emit (Sub)                = puts ("sub")
    | emit (Mult)               = puts ("mult")
    | emit (Div)                = puts ("div")
    | emit (InvokeStk count)    = puts ("invokeStk " ^ Int.toString count)
    | emit (ListIndexImm count) = puts ("listIndexImm " ^ Int.toString count)
    | emit (ListLength)         = puts ("listLength")
    | emit (Nsupvar varname)    = puts ("nsupvar " ^ varname)
    | emit (Label name)         = puts ("label " ^ name)
    | emit (Jump label)         = puts ("jump " ^ label)
    | emit (JumpTrue label)     = puts ("jumpTrue " ^ label)
    | emit (JumpFalse label)    = puts ("jumpFalse " ^ label)
    | emit (LoadArray varname)  = puts ("loadArray " ^ varname)

  fun emitMV SV = ()
    | emitMV MV = emit (List 1)

  fun proc procName args body = (
        puts ("proc " ^ procName ^ " {" ^ (String.concatWith " " args) ^ "} {");
        puts "::tcl::unsupported::assemble {";
        body ();
        puts "}}")

  fun findFun (name, []) = NONE
    | findFun (name, (fundef as Fun (_, funName, _))::fundefs) = 
        if name = funName then SOME fundef
        else findFun (name, fundefs)

  fun varsOf (VarPat (_, var)) = [var]
    | varsOf (WildPat _) = []
    | varsOf (DotsPat _) = []
    | varsOf (IntPat _) = []
    | varsOf (StrPat _) = []
    | varsOf (ListPat (_, pats)) = varsOf' pats
  and varsOf' pats =
        List.foldr (fn (pat, vars) => varsOf pat @ vars) [] pats

  fun compile (fundefs, program, globalVals, globalFuns, sourcemap) =
  let
    fun emitError (msg, NONE) = (
          emit (PushStr "error");
          emit (PushStr msg);
          emit (InvokeStk 2))
      | emitError (msg, SOME span) =
          let
            val spanStr = AntlrStreamPos.spanToString sourcemap span
          in
            emit (PushStr "error");
            emit (PushStr (spanStr ^ ": " ^ msg));
            emit (InvokeStk 2)
          end

    fun compilePat (VarPat (span, v0)) = "{matchvar " ^ v0 ^ "}"
      | compilePat (WildPat (span)) = "matchwild"
      | compilePat (DotsPat (span)) = "matchdots"
      | compilePat (IntPat (span, v0)) = "{matchint "  ^ Int.toString v0 ^ "}"
      | compilePat (StrPat (span, v0)) = "{matchstr {" ^ v0 ^ "}}"
      | compilePat (ListPat (span, v0)) = "{matchlist {" ^ compilePat' v0 ^ "}}"
    and compilePat' xs = String.concatWith " " (List.map compilePat xs)

    val beginProcName = Alpha.gensym "begin"
    val endProcName = Alpha.gensym "end"
    fun compileProgram (Program (span, v0, v1)) =
          (compileTopLevel' "__default" v0; compileNameTopLevel' v1)
    and compileTopLevel' label xs = List.app (compileTopLevel label) xs
    and compileNameTopLevel' xs = List.app compileNameTopLevel xs
    and compileNameTopLevel (NameTopLevel (span, Parse.Ast.Label (span', label), topLevels)) =
          compileTopLevel' label topLevels
    and compileTopLevel label (PatBody (span, pats, guard, body)) =
          (* TODO: use pats and guard *)
          let
            val patBodyProcName = Alpha.gensym label
          in
            proc patBodyProcName ["args"] (fn () => (compileBody body))
          end
      | compileTopLevel label (Begin (span, body)) =
          proc beginProcName [] (fn () => (compileBody body))
      | compileTopLevel label (End (span, body)) =
          proc endProcName [] (fn () => (compileBody body))
      | compileTopLevel label (GlobalVal (span, (valDef as Val (span', pats, exp)))) =
          let
            val scopeProcName = Alpha.gensym "scope"
            val vars = varsOf' pats
          in
            puts ("# " ^ showValDef valDef);
            proc scopeProcName [] (fn () => (
              (* upvar global variables; corresponds to global command *)
              List.app importGlobalVal vars;
              compileValDef valDef));
            (* evaluate in the scope *)
            puts scopeProcName
          end
      | compileTopLevel label (GlobalFun (span, v0)) = compileFunDef v0
    and compileGuard (NoGuard span) = raise Fail "unimplemented"
      | compileGuard (Guard (span, largeExp)) = raise Fail "unimplemented"
    and compileValDef (Val (span, pats, exp)) =
          let
            val failLabel = Alpha.gensym "fail"
            val endLabel = Alpha.gensym "end"
            val vars = varsOf' pats
          in
            (* do match *)
            emit (PushStr "::sheepruntime::match");
            emit (PushStr (compilePat' pats));
            compileLargeExp MV exp;
            emit (InvokeStk 3);
            emit (JumpFalse failLabel);
            (* update variables if the match has succeeded *)
            List.app
              (fn var => (
                emit (PushStr var);
                emit (LoadArray "__bindings");
                emit (Store var);
                emit Pop))
              vars;
            emit (Jump endLabel);
            (* emit an error if the match failed *)
            emit (Label failLabel);
            emitError ("match failed", SOME span);
            emit Pop;
            emit (Label endLabel)
          end
    and compileFunDef (Fun (span, funName, funBodies)) =
            proc funName ["args"] (fn () =>
              let
                val endLabel = Alpha.gensym "end"
              in
                List.app (compileFunBody endLabel) funBodies;
                (* go here if no funBodies matched *)
                emitError ("match failed in function " ^ funName, SOME span);
                (* go here if any matched *)
                emit (Label endLabel)
              end)
    and compileFunBody endLabel (FunBody (span, pats, largeExp)) =
          let
            val nomatchLabel = Alpha.gensym "nomatch"
            val vars = varsOf' pats
          in
            (* do match *)
            emit (PushStr "::sheepruntime::match");
            emit (PushStr (compilePat' pats));
            emit (Load "args");
            emit (InvokeStk 3);
            emit (JumpFalse nomatchLabel);
            (* match succeeded *)
            List.app
              (fn var => (
                emit (PushStr var);
                emit (LoadArray "__bindings");
                emit (Store var);
                emit Pop))
              vars;
            compileLargeExp MV largeExp;
            emit (Jump endLabel);
            (* match failed *)
            emit (Label nomatchLabel)
          end
    and compileBody (Body (span, statements)) =
          compileStatement' MV (Alpha.gensym "end") statements
    and compileStatement' SV endLabel [] =
          (emitError ("no return value within SV", NONE); emit (Label endLabel))
      | compileStatement' MV endLabel [] =
          (emit (List 0); emit (Label endLabel))
      | compileStatement' ctx endLabel (FunStatement (span, v0)::xs) =
          raise Fail "FunStatement should be eliminated at Closure phase"
      | compileStatement' ctx endLabel ((x as ValStatement (span, valDef as Val (span', v0, v1)))::xs) = (
          puts ("# " ^ showStatement x);
          compileValDef valDef;
          compileStatement' ctx endLabel xs)
      | compileStatement' ctx endLabel (NextStatement (span, pats, largeExp)::xs) =
          raise Fail "unimplemented"
      | compileStatement' ctx endLabel (BangStatement (span, largeExp)::xs) =
          raise Fail "unimplemented"
      | compileStatement' ctx endLabel (BangStatement2 (span, exp, exps)::xs) =
          raise Fail "unimplemented"
      | compileStatement' ctx endLabel (ForStatement (span, pats, largeExp, statements)::xs) =
          raise Fail "unimplemented"
      | compileStatement' SV endLabel ((x as ReturnStatement0 span)::xs) = (
          puts ("# " ^ showStatement x);
          emitError ("no return value with in single value context", SOME span);
          emit (Jump endLabel);
          compileStatement' SV endLabel xs)
      | compileStatement' MV endLabel ((x as ReturnStatement0 span)::xs) = (
          puts ("# " ^ showStatement x);
          emit (List 0);
          emit (Jump endLabel);
          compileStatement' MV endLabel xs)
      | compileStatement' SV endLabel ((x as ReturnStatement1 (span, v0))::xs) = (
          puts ("# " ^ showStatement x);
          compileLargeExp SV v0;
          emit (Jump endLabel);
          compileStatement' SV endLabel xs)
      | compileStatement' MV endLabel ((x as ReturnStatement1 (span, v0))::xs) = (
          puts ("# " ^ showStatement x);
          compileLargeExp MV v0;
          emit (Jump endLabel);
          compileStatement' MV endLabel xs)
      | compileStatement' SV endLabel ((x as ReturnStatement2 (span, v0, v1))::xs) = (
          puts ("# " ^ showStatement x);
          compileExp SV v0;
          (* Within single value context, the rest of arguments are evaluated
             and disposed. So it does not matter whether they are evaluated in
             SV or MV. *)
          compileExp' SV v1;
          List.app (fn _ => emit Pop) v1;
          emit (Jump endLabel);
          compileStatement' SV endLabel xs)
      | compileStatement' MV endLabel ((x as ReturnStatement2 (span, v0, v1))::xs) = (
          puts ("# " ^ showStatement x);
          compileExp SV v0;
          compileExp' SV v1; (* TODO: should create MV context in some sense *)
          emit (List (1 + length v1));
          emit (Jump endLabel);
          compileStatement' MV endLabel xs)
      | compileStatement' ctx endLabel ((x as ClosureStatement (span, funName, fvs))::xs) = (
          puts ("# " ^ showStatement x);
          compileClosure (funName, fvs);
          compileStatement' ctx endLabel xs)
    and compileLargeExp ctx (PipeExp (span, largeExp0, largeExp1)) =
          raise Fail "unimplemented"
      | compileLargeExp ctx (AppExp (span, Exp (span', VarExp (span'', v0)), v1)) =
          compileApp (ctx, v0, v1)
      | compileLargeExp ctx (AppExp (span, v0, v1)) =
          raise Fail "1st arg of AppExp should become VarExp at Alpha phase"
      | compileLargeExp ctx (Exp (span, v0)) = compileExp ctx v0
    and compileExp ctx (FunExp (span, v0, v1)) =
          raise Fail "FunExp should be eliminated at Alpha phase"
      | compileExp ctx (EqExp (span, v0, v1)) = compileBinOp (ctx, Eq, v0, v1)
      | compileExp ctx (GtExp (span, v0, v1)) = compileBinOp (ctx, Gt, v0, v1)
      | compileExp ctx (LtExp (span, v0, v1)) = compileBinOp (ctx, Lt, v0, v1)
      | compileExp ctx (GeExp (span, v0, v1)) = compileBinOp (ctx, Ge, v0, v1)
      | compileExp ctx (LeExp (span, v0, v1)) = compileBinOp (ctx, Le, v0, v1)
      | compileExp ctx (ConsExp (span, v0, v1)) = raise Fail "unimplemented"
      | compileExp ctx (AddExp (span, v0, v1)) = compileBinOp (ctx, Add, v0, v1)
      | compileExp ctx (SubExp (span, v0, v1)) = compileBinOp (ctx, Sub, v0, v1)
      | compileExp ctx (MulExp (span, v0, v1)) = compileBinOp (ctx, Mult, v0, v1)
      | compileExp ctx (DivExp (span, v0, v1)) = compileBinOp (ctx, Div, v0, v1)
      | compileExp ctx (App2Exp (span, VarExp (span', v0), v1)) =
          compileApp (ctx, v0, v1)
      | compileExp ctx (App2Exp (span, v0, v1)) =
          raise Fail "1st arg of App2Exp should become VarExp at Alpha phase"
      | compileExp ctx (App3Exp (span, VarExp (span', v0))) =
          compileApp (ctx, v0, [])
      | compileExp ctx (App3Exp (span, v0)) =
          raise Fail "arg of App3Exp should become VarExp at Alpha phase"
      | compileExp ctx (VarExp (span, v0)) = (emit (Load v0); emitMV ctx)
      | compileExp ctx (IntExp (span, v0)) = (emit (PushInt v0); emitMV ctx)
      | compileExp ctx (StrExp (span, v0)) =
          (emit (PushStr "%str"); emit (PushStr v0); emit (List 2); emitMV ctx)
      | compileExp ctx (DotsExp (span)) = raise Fail "unimplemented"
      | compileExp ctx (ListExp (span, exps)) = (
          emit (PushStr "%lst");
          compileExp' SV exps; (* TODO: should create MV context in some sense *)
          emit (List (1 + length exps));
          emitMV ctx)
      | compileExp ctx (NilExp (span)) =
          (emit (PushStr "%lst"); emit (List 1); emitMV ctx)
      | compileExp ctx (BlockExp (span, statements)) =
          compileStatement' ctx (Alpha.gensym "end") statements
      | compileExp ctx (LargeExp (span, v0)) = compileLargeExp ctx v0
    and compileExp' ctx xs = List.app (compileExp ctx) xs
    (* utility functions follow *)
    and compileBinOp (ctx, opcode, v0, v1) =
          (compileExp SV v0; compileExp SV v1; emit opcode; emitMV ctx)
    and compileApp (ctx, funName, exps) = (
          if Global.mem (funName, globalFuns) then (
            emit (PushStr funName);
            compileExp' SV exps; (* TODO: should create MV context in some sense *)
            emit (InvokeStk (1 + length exps)))
          else
            let
              val nofvLabel = Alpha.gensym "nofv"
              val endLabel = Alpha.gensym "end"
            in
              emit (Load funName);   (* load closure record to the stack *)
              emit (ListIndexImm 1); (* get global function name *)
              emit (Load funName);
              emit (ListIndexImm 2); (* get free variables *)
              emit (PushStr "%lst");
              emit (Eq);
              emit (JumpTrue nofvLabel);
              emit (Load funName);
              emit (ListIndexImm 2); (* get free variables again *)
              compileExp' SV exps; (* TODO: should create MV context in some sense *)
              emit (InvokeStk (2 + length exps));
              emit (Jump endLabel);
              emit (Label nofvLabel);
              compileExp' SV exps; (* TODO: should create MV context in some sense *)
              emit (InvokeStk (1 + length exps));
              emit (Label endLabel)
            end;
          (* A function returns multiple results. Pick the first within single
             value context. *)
          case ctx of
               SV => emit (ListIndexImm 0)
             | MV => ())
    and compileClosure (funName, fvExps) = (
          emit (PushStr "%cls");
          emit (PushStr funName);
          emit (PushStr "%lst");
          compileExp' SV fvExps;
          emit (List (1 + length fvExps));
          emit (List 3);
          emit (Store funName);
          emit Pop)
    and importGlobalVal name = (
          emit (PushStr "::");
          emit (PushStr name);
          emit (Nsupvar name);
          emit Pop)
  in
    List.map (fn fundef => compileFunDef fundef) fundefs;
    compileProgram program
  end
end
