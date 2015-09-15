structure Closure = struct
  open Parse.Ast

  (* utility combinators *)
  fun convertUnary ctor convertConstituent topFuns (span, v0) =
        let
          val (topFuns', v0') = convertConstituent topFuns v0
        in
          (topFuns', ctor (span, v0'))
        end
  fun convertBinary ctor convertConstituent topFuns (span, v0, v1) =
        let
          val (topFuns', v0') = convertConstituent topFuns v0
          val (topFuns'', v1') = convertConstituent topFuns' v1
        in
          (topFuns'', ctor (span, v0', v1'))
        end
  fun convertBinary2
          ctor
          (convertConstituent0, convertConstituent1)
          topFuns
          (span, v0, v1) =
        let
          val (topFuns', v0') = convertConstituent0 topFuns v0
          val (topFuns'', v1') = convertConstituent1 topFuns' v1
        in
          (topFuns'', ctor (span, v0', v1'))
        end
  fun convertTernary
          ctor
          (convertConstituent0, convertConstituent1, convertConstituent2)
          topFuns
          (span, v0, v1, v2) =
        let
          val (topFuns', v0') = convertConstituent0 topFuns v0
          val (topFuns'', v1') = convertConstituent1 topFuns' v1
          val (topFuns''', v2') = convertConstituent2 topFuns'' v2
        in
          (topFuns''', ctor (span, v0', v1', v2'))
        end
  fun convertList convertConstituent topFuns xs =
        let
          fun f (x, (topFuns, xs)) =
                let
                  val (topFuns', x') = convertConstituent topFuns x
                in
                  (topFuns', x'::xs)
                end
        in
          List.foldr f (topFuns, []) xs
        end

  fun convert (program, globalVals, globalFuns) : fundef list * program =
  let
    fun convertProgram topFuns (Program (span, v0, v1)) =
          convertBinary2 Program (convertTopLevel', convertNameTopLevel')
            topFuns (span, v0, v1)
    and convertTopLevel' topFuns xs =
          convertList convertTopLevel topFuns xs
    and convertNameTopLevel' topFuns xs =
          convertList convertNameTopLevel topFuns xs
    and convertNameTopLevel topFuns (NameTopLevel (span, v0, v1)) =
          convertBinary2 NameTopLevel (convertLabel, convertTopLevel')
            topFuns (span, v0, v1)
    and convertLabel topFuns (Label (span, v0)) = (topFuns, Label (span, v0))
    and convertTopLevel topFuns (PatBody (span, v0, v1, v2)) =
          convertTernary PatBody (convertPat', convertGuard, convertBody)
            topFuns (span, v0, v1, v2)
      | convertTopLevel topFuns (Begin (span, v0)) =
          convertUnary Begin convertBody topFuns (span, v0)
      | convertTopLevel topFuns (End (span, v0)) =
          convertUnary End convertBody topFuns (span, v0)
      | convertTopLevel topFuns (GlobalVal (span, v0)) =
          convertUnary GlobalVal convertValDef topFuns (span, v0)
      | convertTopLevel topFuns (GlobalFun (span, v0)) =
          convertUnary GlobalFun convertFunDef topFuns (span, v0)
    and convertGuard topFuns (NoGuard (span)) = (topFuns, NoGuard span)
      | convertGuard topFuns (Guard (span, v0)) =
          convertUnary Guard convertLargeExp topFuns (span, v0)
    and convertValDef topFuns (Val (span, v0, v1)) =
          convertBinary2 Val (convertPat', convertLargeExp) topFuns (span, v0, v1)
    and convertFunDef topFuns (Fun (span, v0, v1)) =
          let
            fun f topFuns x = (topFuns, x)
          in
            convertBinary2 Fun (f, convertFunBody') topFuns (span, v0, v1)
          end
    and convertFunBody topFuns (FunBody (span, v0, v1)) =
          convertBinary2 FunBody (convertPat', convertLargeExp)
            topFuns (span, v0, v1)
    and convertFunBody' topFuns xs = convertList convertFunBody topFuns xs
    and convertPat topFuns (VarPat (span, v0)) = (topFuns, VarPat (span, v0))
      | convertPat topFuns (WildPat (span)) = (topFuns, WildPat span)
      | convertPat topFuns (DotsPat (span)) = (topFuns, DotsPat span)
      | convertPat topFuns (IntPat (span, v0)) = (topFuns, IntPat (span, v0))
      | convertPat topFuns (StrPat (span, v0)) = (topFuns, StrPat (span, v0))
      | convertPat topFuns (ListPat (span, v0)) =
          convertUnary ListPat convertPat' topFuns (span, v0)
    and convertPat' topFuns xs = convertList convertPat topFuns xs
    and convertBody topFuns (Body (span, v0)) =
          convertUnary Body convertStatement' topFuns (span, v0)
    and convertStatement' topFuns [] = (topFuns, [])
      | convertStatement' topFuns (FunStatement (span, funDef as Fun (span', funName, funBodies))::xs) =
          (* Replace FunStatement with ClosureStatement
             and move the function top level *)
          let
            val freeVars = Fv.fvFunDef (globalVals @ globalFuns) funDef
            (* a closure is a pair of global function name and actual free vars *)
            val closure =
                  let
                    val freeVarExps = map (fn v => VarExp (span', v)) freeVars
                  in
                    ClosureStatement (span, funName, freeVarExps)
                  end
            (* closure-convert function bodies *)
            val (topFuns', funBodies') = convertFunBody' topFuns funBodies
            (* prepend the free vars to formal arguments of the function bodies *)
            fun addFormalFreeVars (FunBody (span, pats, largeExp)) =
                  let
                    val freeVarPats = map (fn v => VarPat (span', v)) freeVars
                  in
                    case freeVarPats of
                         [] =>
                           FunBody (span, pats, largeExp)
                       | _::_ =>
                           FunBody (span, ListPat (span', freeVarPats)::pats, largeExp)
                  end
            val funBodies'' = map addFormalFreeVars funBodies'
            (* move the function to top level *)
            val topFuns'' = Fun (span', funName, funBodies'')::topFuns'
            val (topFuns''', xs') = convertStatement' topFuns'' xs
          in
            (topFuns''', closure::xs')
          end
      | convertStatement' topFuns (ValStatement (span, v0)::xs) =
          let
            val (topFuns', x) = convertUnary ValStatement convertValDef topFuns (span, v0)
            val (topFuns'', xs') = convertStatement' topFuns' xs
          in
            (topFuns'', x::xs')
          end
      | convertStatement' topFuns (NextStatement (span, v0, v1)::xs) =
          let
            val (topFuns', x) = convertBinary2 NextStatement (convertPat', convertLargeExp) topFuns (span, v0, v1)
            val (topFuns'', xs') = convertStatement' topFuns' xs
          in
            (topFuns'', x::xs')
          end
      | convertStatement' topFuns (BangStatement (span, v0)::xs) =
          let
            val (topFuns', x) = convertUnary BangStatement convertLargeExp topFuns (span, v0)
            val (topFuns'', xs') = convertStatement' topFuns' xs
          in
            (topFuns'', x::xs')
          end
      | convertStatement' topFuns (BangStatement2 (span, v0, v1)::xs) =
          let
            val (topFuns', x) = convertBinary2 BangStatement2 (convertExp, convertExp') topFuns (span, v0, v1)
            val (topFuns'', xs') = convertStatement' topFuns' xs
          in
            (topFuns'', x::xs')
          end
      | convertStatement' topFuns (ForStatement (span, v0, v1, v2)::xs) =
          let
            val (topFuns', x) = convertTernary ForStatement (convertPat', convertLargeExp, convertStatement') topFuns (span, v0, v1, v2)
            val (topFuns'', xs') = convertStatement' topFuns' xs
          in
            (topFuns'', x::xs')
          end
      | convertStatement' topFuns ((x as ReturnStatement0 span)::xs) =
          let
            val (topFuns', xs') = convertStatement' topFuns xs
          in
            (topFuns', x::xs')
          end
      | convertStatement' topFuns (ReturnStatement1 (span, v0)::xs) =
          let
            val (topFuns', x) = convertUnary ReturnStatement1 convertLargeExp topFuns (span, v0)
            val (topFuns'', xs') = convertStatement' topFuns' xs
          in
            (topFuns'', x::xs')
          end
      | convertStatement' topFuns (ReturnStatement2 (span, v0, v1)::xs) =
          let
            val (topFuns', x) = convertBinary2 ReturnStatement2 (convertExp, convertExp') topFuns (span, v0, v1)
            val (topFuns'', xs') = convertStatement' topFuns' xs
          in
            (topFuns'', x::xs')
          end
      | convertStatement' topFuns (ClosureStatement (span, v0, v1)::xs) =
          raise Fail "closure statement must not used explicitly"
    and convertLargeExp topFuns (PipeExp (span, v0, v1)) =
          convertBinary PipeExp convertLargeExp topFuns (span, v0, v1)
      | convertLargeExp topFuns (AppExp (span, v0, v1)) =
          convertBinary2 AppExp (convertLargeExp, convertExp')
            topFuns (span, v0, v1)
      | convertLargeExp topFuns (Exp (span, v0)) =
          convertUnary Exp convertExp topFuns (span, v0)
    and convertExp topFuns (FunExp (span, v0, v1)) =
          raise Fail "FunExp should be eliminated at Alpha phase"
      | convertExp topFuns (EqExp (span, v0, v1)) =
          convertBinary EqExp convertExp topFuns (span, v0, v1)
      | convertExp topFuns (GtExp (span, v0, v1)) =
          convertBinary GtExp convertExp topFuns (span, v0, v1)
      | convertExp topFuns (LtExp (span, v0, v1)) =
          convertBinary LtExp convertExp topFuns (span, v0, v1)
      | convertExp topFuns (GeExp (span, v0, v1)) =
          convertBinary GeExp convertExp topFuns (span, v0, v1)
      | convertExp topFuns (LeExp (span, v0, v1)) =
          convertBinary LeExp convertExp topFuns (span, v0, v1)
      | convertExp topFuns (ConsExp (span, v0, v1)) =
          convertBinary ConsExp convertExp topFuns (span, v0, v1)
      | convertExp topFuns (AddExp (span, v0, v1)) =
          convertBinary AddExp convertExp topFuns (span, v0, v1)
      | convertExp topFuns (SubExp (span, v0, v1)) =
          convertBinary SubExp convertExp topFuns (span, v0, v1)
      | convertExp topFuns (MulExp (span, v0, v1)) =
          convertBinary MulExp convertExp topFuns (span, v0, v1)
      | convertExp topFuns (DivExp (span, v0, v1)) =
          convertBinary DivExp convertExp topFuns (span, v0, v1)
      | convertExp topFuns (App2Exp (span, v0, v1)) =
          convertBinary2 App2Exp (convertExp, convertExp') topFuns (span, v0, v1)
      | convertExp topFuns (App3Exp (span, v0)) =
          convertUnary App3Exp convertExp topFuns (span, v0)
      | convertExp topFuns (VarExp (span, v0)) = (topFuns, VarExp (span, v0))
      | convertExp topFuns (IntExp (span, v0)) = (topFuns, IntExp (span, v0))
      | convertExp topFuns (StrExp (span, v0)) = (topFuns, StrExp (span, v0))
      | convertExp topFuns (DotsExp (span)) = (topFuns, DotsExp span)
      | convertExp topFuns (ListExp (span, v0)) =
          convertUnary ListExp convertExp' topFuns (span, v0)
      | convertExp topFuns (NilExp (span)) = (topFuns, NilExp span)
      | convertExp topFuns (BlockExp (span, v0)) =
          convertUnary BlockExp convertStatement' topFuns (span, v0)
      | convertExp topFuns (LargeExp (span, v0)) =
          convertUnary LargeExp convertLargeExp topFuns (span, v0)
    and convertExp' topFuns xs = convertList convertExp topFuns xs
  in
    convertProgram [] program
  end
end
