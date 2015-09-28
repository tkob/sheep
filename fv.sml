structure Fv = struct
  open Parse.Ast

  structure S = SetFun(type elem = string)

  fun varsOfPat' pats =
        List.foldr (fn (pat, vars) => S.union (varsOfPat pat) vars) S.empty pats
  and varsOfPat (VarPat (span, var)) = S.singleton var
    | varsOfPat (WildPat span) = S.empty
    | varsOfPat (DotsPat span) = S.empty
    | varsOfPat (IntPat span) = S.empty
    | varsOfPat (StrPat span) = S.empty
    | varsOfPat (ListPat (span, pats)) = varsOfPat' pats

  fun fvLargeExp env (PipeExp (span, v0, v1)) =
        S.union (fvLargeExp env v0) (fvLargeExp env v1)
    | fvLargeExp env (AppExp (span, v0, v1)) =
        S.union (fvLargeExp env v0) (fvExp' env v1)
    | fvLargeExp env (Exp (span, v0)) =
        fvExp env v0
  and fvExp env (FunExp (span, v0, v1)) =
        raise Fail "FunExp should be eliminated at Alpha phase"
    | fvExp env (EqExp (span, v0, v1)) = S.union (fvExp env v0) (fvExp env v1)
    | fvExp env (GtExp (span, v0, v1)) = S.union (fvExp env v0) (fvExp env v1)
    | fvExp env (LtExp (span, v0, v1)) = S.union (fvExp env v0) (fvExp env v1)
    | fvExp env (GeExp (span, v0, v1)) = S.union (fvExp env v0) (fvExp env v1)
    | fvExp env (LeExp (span, v0, v1)) = S.union (fvExp env v0) (fvExp env v1)
    | fvExp env (ConsExp (span, v0, v1)) = S.union (fvExp env v0) (fvExp env v1)
    | fvExp env (AddExp (span, v0, v1)) = S.union (fvExp env v0) (fvExp env v1)
    | fvExp env (SubExp (span, v0, v1)) = S.union (fvExp env v0) (fvExp env v1)
    | fvExp env (MulExp (span, v0, v1)) = S.union (fvExp env v0) (fvExp env v1)
    | fvExp env (DivExp (span, v0, v1)) = S.union (fvExp env v0) (fvExp env v1)
    | fvExp env (App2Exp (span, v0, v1)) =
        S.union (fvExp env v0) (fvExp' env v1)
    | fvExp env (App3Exp (span, v0)) =
        fvExp env v0
    | fvExp env (VarExp (span, v0)) =
        if S.member env v0 then S.empty else S.singleton v0
    | fvExp env (IntExp (span, v0)) = S.empty
    | fvExp env (StrExp (span, v0)) = S.empty
    | fvExp env (TrueExp (span)) = S.empty
    | fvExp env (FalseExp (span)) = S.empty
    | fvExp env (ListExp (span, v0)) = fvExp' env v0
    | fvExp env (NilExp (span)) = S.empty
    | fvExp env (BlockExp (span, v0)) = fvStatement' env v0
    | fvExp env (LargeExp (span, v0)) = fvLargeExp env v0
  and fvExp' env xs = List.foldr (fn (x, s) => S.union (fvExp env x) s) S.empty xs
  and fvStatement' env [] = S.empty
    | fvStatement' env (FunStatement (span, Fun (span', v0, v1))::xs) =
        let
          val env' = S.union (S.singleton v0) env
          val vars = fvFunBody' env' v1
        in
          S.union vars (fvStatement' env' xs)
        end
    | fvStatement' env (ValStatement (span, Val (span', v0, v1))::xs) =
        let
          val boundVars = varsOfPat' v0
          val env' = S.union boundVars env
        in
          S.union (fvLargeExp env v1) (fvStatement' (S.union boundVars env) xs)
        end
    | fvStatement' env (NextStatement (span, v0, v1)::xs) =
        (* patterns in a next statement are not counted as free variables *)
        S.union (fvLargeExp env v1) (fvStatement' env xs)
    | fvStatement' env (BangStatement (span, v0)::xs) =
        S.union (fvLargeExp env v0) (fvStatement' env xs)
    | fvStatement' env (BangStatement2 (span, v0, v1)::xs) =
        S.union (fvExp env v0) (S.union (fvExp' env v1) (fvStatement' env xs))
    | fvStatement' env (ForStatement (span, v0, v1, v2)::xs) =
        let
          val vars1 = fvLargeExp env v1
          val boundVars = varsOfPat' v0
          val vars2 = fvStatement' (S.union boundVars env) v2
          val vars = S.union vars1 vars2
        in
          S.union vars (fvStatement' env xs)
        end
    | fvStatement' env (ReturnStatement0 span::xs) = S.empty
    | fvStatement' env (ReturnStatement1 (span, v0)::xs) =
        S.union (fvLargeExp env v0) (fvStatement' env xs)
    | fvStatement' env (ReturnStatement2 (span, v0, v1)::xs) =
        S.union (fvExp env v0) (S.union (fvExp' env v1) (fvStatement' env xs))
    | fvStatement' env (ClosureStatement (span, v0, v1)::xs) =
        let
          val env' = S.insert env v0
        in
          S.union (fvExp' env v1) (fvStatement' env' xs)
        end
  and fvFunBody' env xs =
        List.foldr (fn (x, s) => S.union (fvFunBody env x) s) S.empty xs
  and fvFunBody env (FunBody (span, v0, v1)) =
        let
          val boundVars = varsOfPat' v0
          val env' = S.union boundVars env
        in
          fvLargeExp env' v1
        end

  fun fvFunDef vars (Fun (span, funName, funBodies)) =
        let
          val env = List.foldr (fn (e, s) => S.insert s e) S.empty vars
        in
          S.toList (fvFunBody' (S.insert env funName) funBodies)
        end

  fun fvValDef vars (Val (span, pats, largeExp)) =
        let
          val env = List.foldr (fn (e, s) => S.insert s e) S.empty vars
        in
          S.toList (fvLargeExp env largeExp)
        end

  fun fvStatements vars statements =
        let
          val env = List.foldr (fn (e, s) => S.insert s e) S.empty vars
        in
          S.toList (fvStatement' env statements)
        end
end
