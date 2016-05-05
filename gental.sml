(* Code Generator for Tcl Assembly Language *)
structure GenTal = struct
  open Parse.Ast

  datatype opcode = PushInt of int
                  | PushStr of string
                  | PushTrue
                  | PushFalse
                  | Pop
                  | Load of string
                  | Store of string
                  | Dup
                  | Reverse of int
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
                  | ListIndex
                  | ListIndexImm of int
                  | ListLength
                  | ListConcat
                  | Lappend of string
                  | Nsupvar of string
                  | Label of string
                  | Jump of string
                  | JumpTrue of string
                  | JumpFalse of string
                  | BeginCatch of string
                  | EndCatch
                  | LoadArray of string
                  | StoreArray of string
                  | Incr of string
                  | IncrImm of string * int

  datatype context = SV | MV (* multi-value context *)

  fun puts s = (print s; print "\n")
  fun iputs s = (print "    "; puts s)
  fun comment s = (print "    # "; puts s)

  fun emit (PushInt value)      = iputs ("push " ^ Int.toString value)
    | emit (PushStr value)      = iputs ("push {" ^ value ^ "}")
    | emit (PushTrue)           = iputs ("push true")
    | emit (PushFalse)          = iputs ("push false")
    | emit (Pop)                = iputs ("pop")
    | emit (Load varname)       = iputs ("load " ^ varname)
    | emit (Store varname)      = iputs ("store " ^ varname)
    | emit (Dup)                = iputs ("dup")
    | emit (Reverse count)      = iputs ("reverse " ^ Int.toString count)
    | emit (List len)           = iputs ("list " ^ Int.toString len)
    | emit (Eq)                 = iputs ("eq")
    | emit (Gt)                 = iputs ("gt")
    | emit (Lt)                 = iputs ("lt")
    | emit (Ge)                 = iputs ("ge")
    | emit (Le)                 = iputs ("le")
    | emit (Add)                = iputs ("add")
    | emit (Sub)                = iputs ("sub")
    | emit (Mult)               = iputs ("mult")
    | emit (Div)                = iputs ("div")
    | emit (InvokeStk count)    = iputs ("invokeStk " ^ Int.toString count)
    | emit (ListIndex)          = iputs ("listIndex")
    | emit (ListIndexImm count) = iputs ("listIndexImm " ^ Int.toString count)
    | emit (ListLength)         = iputs ("listLength")
    | emit (ListConcat)         = iputs ("listConcat")
    | emit (Lappend varname)    = iputs ("lappend " ^ varname)
    | emit (Nsupvar varname)    = iputs ("nsupvar " ^ varname)
    | emit (Label name)         =  puts ("label " ^ name)
    | emit (Jump label)         = iputs ("jump " ^ label)
    | emit (JumpTrue label)     = iputs ("jumpTrue " ^ label)
    | emit (JumpFalse label)    = iputs ("jumpFalse " ^ label)
    | emit (BeginCatch label)   = iputs ("beginCatch " ^ label)
    | emit (EndCatch)           = iputs ("endCatch")
    | emit (LoadArray varname)  = iputs ("loadArray " ^ varname)
    | emit (StoreArray varname) = iputs ("storeArray " ^ varname)
    | emit (Incr varname)       = iputs ("incr " ^ varname)
    | emit (IncrImm (varName, imm8)) =
        iputs ("incrImm " ^ varName ^ " " ^ Int.toString imm8)

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

    fun compilePats (pats, expf, nomatchLabel) = (
          (* do match *)
          comment "pattern matching begins";
          emit (PushStr "::sheepruntime::match");
          emit (PushStr (compilePat' pats));
          expf ();
          emit (InvokeStk 3);
          emit (JumpFalse nomatchLabel);
          comment "pattern matching end")

    fun compilePatsLocal (pats, expf, nomatchLabel) =
          let
            val vars = varsOf' pats
          in
            compilePats (pats, expf, nomatchLabel);
            (* match succeeded *)
            if length vars > 0 then (
              comment "match succeeded, reading matched variables";
              List.app
                (fn var => (
                  emit (PushStr var);
                  emit (LoadArray "__bindings");
                  emit (Store var);
                  emit Pop))
                vars;
              comment "have read matched variables")
            else ()
          end

    fun compilePatsArray (pats, expf, arrayName, nomatchLabel) =
          let
            val vars = varsOf' pats
          in
            compilePats (pats, expf, nomatchLabel);
            (* match succeeded *)
            if length vars > 0 then (
              comment "match succeeded, reading matched variables";
              List.app
                (fn var => (
                  emit (PushStr var);
                  emit (PushStr var);
                  emit (LoadArray "__bindings");
                  emit (StoreArray arrayName);
                  emit Pop))
                vars;
              comment "have read matched variables")
            else ()
          end

    val beginProcName = "__BEGIN"
    val endProcName = "__END"
    fun compileProgram (Program (span, v0, v1)) =
          (compileTopLevel' "__default" v0; compileNameTopLevel' v1)
    and compileTopLevel' label xs = List.app (compileTopLevel label) xs
    and compileNameTopLevel' xs = List.app compileNameTopLevel xs
    and compileNameTopLevel (NameTopLevel (span, Parse.Ast.Label (span', label), topLevels)) =
          compileTopLevel' label topLevels
    and compileTopLevel label (PatBody (span, pats, guard, Body (span', statements))) =
          let
            val patBodyProcName = Alpha.gensym label
            val freeVars = Fv.fvStatements [] statements
            val nomatchLabel = Alpha.gensym "nomatch"
            val endLabel = Alpha.gensym "end"
          in
            proc patBodyProcName ["args"] (fn () => (
              List.app importGlobalVal freeVars;
              compilePatsLocal (pats, (fn () => emit (Load "args")), nomatchLabel);
              case guard of
                   NoGuard _ => ()
                 | Guard (span'', largeExp) => (
                     compileLargeExp SV largeExp;
                     emit (JumpFalse nomatchLabel));
              compileStatement' MV (Alpha.gensym "end") statements;
              emit Pop;
              emit PushTrue;
              emit (Jump endLabel);
              emit (Label nomatchLabel);
              emit PushFalse;
              emit (Label endLabel)));
            puts ("lappend ::sheepruntime::__patbody(" ^ label ^ ") " ^ patBodyProcName)
          end
      | compileTopLevel label (Begin (span, Body (span', statements))) =
          let
            val patBodyProcName = Alpha.gensym label
            val freeVars = Fv.fvStatements [] statements
          in
            proc beginProcName [] (fn () => (
              List.app importGlobalVal freeVars;
              compileStatement' MV (Alpha.gensym "end") statements))
          end
      | compileTopLevel label (End (span, Body (span', statements))) =
          let
            val patBodyProcName = Alpha.gensym label
            val freeVars = Fv.fvStatements [] statements
          in
            proc endProcName [] (fn () => (
              List.app importGlobalVal freeVars;
              compileStatement' MV (Alpha.gensym "end") statements))
          end
      | compileTopLevel label (GlobalVal (span, (valDef as Val (span', pats, exp)))) =
          let
            val scopeProcName = Alpha.gensym "scope"
            val vars = varsOf' pats
            val freeVars = Fv.fvValDef [] valDef
          in
            comment (showValDef valDef);
            proc scopeProcName [] (fn () => (
              (* upvar global variables; corresponds to global command *)
              List.app importGlobalVal vars;
              List.app importGlobalVal freeVars;
              compileValDef valDef));
            (* evaluate in the scope *)
            puts scopeProcName
          end
      | compileTopLevel label (GlobalFun (_, funDef as Fun (_, funName, _))) =
          let
            val scopeProcName = Alpha.gensym "scope"
          in
            compileFunDef funDef;
            proc scopeProcName [] (fn () => (
              (* every global function has a companion closure *)
              importGlobalVal funName;
              compileClosure (funName, [])));
            puts scopeProcName
          end
    and compileValDef (Val (span, pats, exp)) =
          let
            val failLabel = Alpha.gensym "fail"
            val endLabel = Alpha.gensym "end"
          in
            compilePatsLocal (pats, (fn () => compileLargeExp MV exp), failLabel);
            emit (Jump endLabel);
            (* emit an error if the match failed *)
            emit (Label failLabel);
            emitError ("match failed", SOME span);
            emit Pop;
            emit (Label endLabel)
          end
    and compileFunDef (funDef as Fun (span, funName, funBodies)) =
            proc funName ["args"] (fn () =>
              let
                val endLabel = Alpha.gensym "end"
                val fvs = Fv.fvFunDef [] funDef
              in
                List.app importGlobalVal fvs;
                List.app (compileFunBody endLabel) funBodies;
                (* go here if no funBodies matched *)
                emitError ("match failed in function " ^ funName, SOME span);
                (* go here if any matched *)
                emit (Label endLabel)
              end)
    and compileFunBody endLabel (FunBody (span, pats, largeExp)) =
          let
            val nomatchLabel = Alpha.gensym "nomatch"
          in
            compilePatsLocal (pats, (fn () => emit (Load "args")), nomatchLabel);
            compileLargeExp MV largeExp;
            emit (Jump endLabel);
            (* match failed *)
            emit (Label nomatchLabel)
          end
    and compileStatement' SV endLabel [] =
          (emitError ("no return value within SV", NONE); emit (Label endLabel))
      | compileStatement' MV endLabel [] =
          (emit (List 0); emit (Label endLabel))
      | compileStatement' ctx endLabel (FunStatement (span, v0)::xs) =
          raise Fail "FunStatement should be eliminated at Closure phase"
      | compileStatement' ctx endLabel ((x as ValStatement (span, valDef as Val (span', v0, v1)))::xs) = (
          comment (showStatement x);
          compileValDef valDef;
          compileStatement' ctx endLabel xs)
      | compileStatement' ctx endLabel (NextStatement (span, pats, largeExp)::xs) =
          let
            val failLabel = Alpha.gensym "fail"
            val endLabel = Alpha.gensym "end"
          in
            compilePatsArray (pats, (fn () => compileLargeExp MV largeExp), "__next", failLabel);
            emit (Jump endLabel);
            (* emit an error if the match failed *)
            emit (Label failLabel);
            emitError ("match failed", SOME span);
            emit Pop;
            emit (Label endLabel)
          end
      | compileStatement' ctx endLabel (BangStatement (span, largeExp)::xs) = (
          importNS ("::sheepruntime", "__!");
          compileLargeExp MV largeExp;
          emit (Lappend "__!");
          emit Pop;
          compileStatement' ctx endLabel xs)
      | compileStatement' ctx endLabel (BangStatement2 (span, exp, exps)::xs) = (
          importNS ("::sheepruntime", "__!");
          compileExp MV exp;
          List.app
              (fn exp => (
                  compileExp MV exp;
                  emit ListConcat))
              exps;
          emit (Lappend "__!");
          emit Pop;
          compileStatement' ctx endLabel xs)
      | compileStatement' ctx endLabel (ForStatement (span, pats, largeExp, statements)::xs) =
          let
            val list = Alpha.gensym "__list"
            val len = Alpha.gensym "__len"
            val i = Alpha.gensym "__i"
            val loopStartLabel = Alpha.gensym "loopstart"
            val loopEndLabel = Alpha.gensym "loopend"
            val nomatchLabel = Alpha.gensym "nomatch"
            val endLabel' = Alpha.gensym "end"
          in
            (* __list = largeExp; __len = length largeExp *)
            compileLargeExp SV largeExp;
            emit (Store list);
            emit (ListLength);
            emit (Store len);
            emit Pop;
            (* __i = 0 *)
            emit (PushInt 0);
            emit (Store i);
            emit Pop;
            (* loop begins *)
            emit (Label loopStartLabel);

            compilePatsLocal (
              pats,
              fn () => (
                List.app
                  (fn _ => (
                    (* create a sublist whose length matched the number of patterns *)
                    emit (Load list);
                    emit (IncrImm (i, 1));
                    emit ListIndex;
                    emit (List (length pats))))
                  pats),
              nomatchLabel);

            (* TODO: 'return' inside for loop means just 'continue' for now *)
            compileStatement' MV endLabel' statements;
            emit Pop;

            (* continue if there remains enough items to match *)
            emit (Load i);
            emit (PushInt (length pats));
            emit Add;
            emit (Load len);
            emit Lt;
            emit (JumpTrue loopStartLabel);
            emit (Jump loopEndLabel);

            (* emit an error if the match failed *)
            emit (Label nomatchLabel);
            emitError ("match failed", SOME span);
            emit Pop;

            emit (Label loopEndLabel);
            compileStatement' ctx endLabel xs
          end
      | compileStatement' SV endLabel ((x as ReturnStatement0 span)::xs) = (
          comment (showStatement x);
          emitError ("no return value with in single value context", SOME span);
          emit (Jump endLabel);
          compileStatement' SV endLabel xs)
      | compileStatement' MV endLabel ((x as ReturnStatement0 span)::xs) = (
          comment (showStatement x);
          emit (List 0);
          emit (Jump endLabel);
          compileStatement' MV endLabel xs)
      | compileStatement' SV endLabel ((x as ReturnStatement1 (span, v0))::xs) = (
          comment (showStatement x);
          compileLargeExp SV v0;
          emit (Jump endLabel);
          compileStatement' SV endLabel xs)
      | compileStatement' MV endLabel ((x as ReturnStatement1 (span, v0))::xs) = (
          comment (showStatement x);
          compileLargeExp MV v0;
          emit (Jump endLabel);
          compileStatement' MV endLabel xs)
      | compileStatement' SV endLabel ((x as ReturnStatement2 (span, v0, v1))::xs) = (
          comment (showStatement x);
          compileExp SV v0;
          (* Within single value context, the rest of arguments are evaluated
             and disposed. So it does not matter whether they are evaluated in
             SV or MV. *)
          compileExp' SV v1;
          List.app (fn _ => emit Pop) v1;
          emit (Jump endLabel);
          compileStatement' SV endLabel xs)
      | compileStatement' MV endLabel ((x as ReturnStatement2 (span, exp, exps))::xs) = (
          comment (showStatement x);
          compileExp MV exp;
          List.app (fn exp => (compileExp MV exp; emit ListConcat)) exps;
          emit (Jump endLabel);
          compileStatement' MV endLabel xs)
      | compileStatement' ctx endLabel ((x as ClosureStatement (span, funName, fvs))::xs) = (
          comment (showStatement x);
          compileClosure (funName, fvs);
          compileStatement' ctx endLabel xs)
    and compileLargeExp ctx (PipeExp (span, largeExp0, largeExp1)) =
          raise Fail "PipeExp should be eliminated at Alpha phase"
      | compileLargeExp ctx (AppExp (span, Exp (span', VarExp (span'', funName)), exps)) = (
          if Global.mem (funName, globalFuns) then (
            emit (PushStr "::sheepruntime::app");
            emit (PushStr funName);
            emit (PushStr "");
            List.app
              (fn exp => (
                compileExp MV exp;
                emit ListConcat))
              exps;
            emit (InvokeStk 3))
          else
            let
              val nofvLabel = Alpha.gensym "nofv"
              val endLabel = Alpha.gensym "end"
            in
              emit (Load funName);
              emit (ListIndexImm 2); (* get free variables *)
              emit (PushStr "%lst");
              emit (Eq);
              emit (JumpTrue nofvLabel);
              emit (PushStr "::sheepruntime::appfv");
              emit (Load funName);   (* load closure record to the stack *)
              emit (ListIndexImm 1); (* get global function name *)
              emit (Load funName);
              emit (ListIndexImm 2); (* get free variables again *)
              emit (PushStr "");
              List.app
                (fn exp => (
                  compileExp MV exp;
                  emit ListConcat))
                exps;
              emit (InvokeStk 4);
              emit (Jump endLabel);
              emit (Label nofvLabel);
              emit (PushStr "::sheepruntime::app");
              emit (Load funName);   (* load closure record to the stack *)
              emit (ListIndexImm 1); (* get global function name *)
              emit (PushStr "");
              List.app
                (fn exp => (
                  compileExp MV exp;
                  emit ListConcat))
                exps;
              emit (InvokeStk 3);
              emit (Label endLabel)
            end;
          (* A function returns multiple results. Pick the first within single
             value context. *)
          case ctx of
               SV => emit (ListIndexImm 0)
             | MV => ())
      | compileLargeExp ctx (AppExp (span, v0, v1)) =
          raise Fail "1st arg of AppExp should become VarExp at Alpha phase"
      | compileLargeExp ctx (Exp (span, v0)) = compileExp ctx v0
    and compileExp ctx (FunExp (span, v0, v1)) =
          raise Fail "FunExp should be eliminated at Alpha phase"
      | compileExp ctx (EqExp (span, v0, v1)) =
          compileBinOpBool (ctx, Eq, v0, v1)
      | compileExp ctx (GtExp (span, v0, v1)) =
          compileBinOpBool (ctx, Gt, v0, v1)
      | compileExp ctx (LtExp (span, v0, v1)) =
          compileBinOpBool (ctx, Lt, v0, v1)
      | compileExp ctx (GeExp (span, v0, v1)) =
          compileBinOpBool (ctx, Ge, v0, v1)
      | compileExp ctx (LeExp (span, v0, v1)) =
          compileBinOpBool (ctx, Le, v0, v1)
      | compileExp ctx (ConsExp (span, v0, v1)) =
          let
            val endLabel = Alpha.gensym "end"
            val failLabel = Alpha.gensym "fail"
          in
            compileExp SV v0;
            emit (List 1);
            compileExp SV v1;
            untagList failLabel;
            emit ListConcat;
            tagList ();
            emit (Jump endLabel);
            emit (Label failLabel);
            emit Pop;
            emitError ("right hand of :: is not a list", SOME span);
            emit Pop;
            emit (Label endLabel);
            emitMV ctx
          end
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
      | compileExp ctx (TrueExp (span)) = (emit PushTrue; emitMV ctx)
      | compileExp ctx (FalseExp (span)) = (emit PushFalse; emitMV ctx)
      | compileExp ctx (ListExp (span, exps)) = (
          emit (PushStr "%lst");
          List.app
              (fn exp => (
                  compileExp MV exp;
                  emit ListConcat))
              exps;
          emitMV ctx)
      | compileExp ctx (NilExp (span)) =
          (emit (PushStr "%lst"); emit (List 1); emitMV ctx)
      | compileExp ctx (BlockExp (span, statements)) =
          compileStatement' ctx (Alpha.gensym "end") statements
      | compileExp ctx (LargeExp (span, v0)) = compileLargeExp ctx v0
    and compileExp' ctx xs = List.app (compileExp ctx) xs
    (* utility functions follow *)
    and compileBinOp' (ctx, opcode, v0, v1, postf) =
          (compileExp SV v0; compileExp SV v1; emit opcode; postf (); emitMV ctx)
    and compileBinOp (ctx, opcode, v0, v1) =
          compileBinOp' (ctx, opcode, v0, v1, fn () => ())
    and compileBinOpBool (ctx, opcode, v0, v1) =
          compileBinOp' (ctx, opcode, v0, v1, tagBool)
    and compileApp (ctx, funName, exps) = (
          if Global.mem (funName, globalFuns) then (
            emit (PushStr funName);
            compileExp' SV exps;
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
              compileExp' SV exps;
              emit (InvokeStk (2 + length exps));
              emit (Jump endLabel);
              emit (Label nofvLabel);
              compileExp' SV exps;
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
    and importNS (ns, name) = (
          emit (PushStr ns);
          emit (PushStr name);
          emit (Nsupvar name);
          emit Pop)
    and untagList failLabel = (
          emit Dup;
          emit (ListIndexImm 0);
          emit (PushStr "%lst");
          emit Eq;
          emit (JumpFalse failLabel);
          emit (PushStr "lrange");
          emit (Reverse 2);
          emit (PushInt 1);
          emit (PushStr "end");
          emit (InvokeStk 4))
    and tagList () = (
          emit (PushStr "%lst");
          emit (Reverse 2);
          emit ListConcat)
    and tagBool () =
          let
            val endLabel = Alpha.gensym "end"
            val falseLabel = Alpha.gensym "false"
          in
            emit (JumpFalse falseLabel);
            emit PushTrue;
            emit (Jump endLabel);
            emit (Label falseLabel);
            emit PushFalse;
            emit (Label endLabel)
          end
  in
    List.map (fn fundef => compileFunDef fundef) fundefs;
    compileProgram program
  end
end
