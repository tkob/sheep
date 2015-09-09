structure Alpha = struct
  open Parse.Ast

  (* generate a brand new name based on a given name *) 
  local
    val suffix = ref 0
  in
    fun gensym name =
          name ^ "." ^ Int.toString (!suffix)
          before suffix := !suffix + 1
  end

  type env = (string * string) list
  val initEnv = []
  fun lookupEnv (name, []) = NONE
    | lookupEnv (name, (n1, n2)::ns) = if name = n1 then SOME n2 else lookupEnv (name, ns)
  fun extendEnv ((n1, n2), ns) = (n1, n2)::ns

  fun mem (x, xs) = List.exists (fn y => y = x) xs

  (* collect global names of a given program *)
  fun globalNamesOfProgram names (Program (span, v0, v1)) =
        globalNamesOfNameTopLevel' (globalNamesOfTopLevel' names v0) v1
  and globalNamesOfTopLevel' names xs =
        List.foldr (fn (x, names) => globalNamesOfTopLevel names x) names xs
  and globalNamesOfNameTopLevel' names xs =
        List.foldr (fn (x, names) => globalNamesOfNameTopLevel names x) names xs
  and globalNamesOfNameTopLevel names (NameTopLevel (span, v0, v1)) =
        (* ignore Label *)
        globalNamesOfTopLevel' names v1
  and globalNamesOfTopLevel names (PatBody (span, v0, v1, v2)) = []
    | globalNamesOfTopLevel names (GlobalVal (span, v0)) =
        globalNamesOfValDef names v0
    | globalNamesOfTopLevel names (GlobalFun (span, v0)) =
        globalNamesOfFunDef names v0
  and globalNamesOfValDef names (Val (span, v0, v1)) =
      globalNamesOfPat' names v0
  and globalNamesOfFunDef names (Fun (span, v0, v1)) =
        if mem (v0, names) then raise Fail "duplicate variable" else v0::names
  and globalNamesOfPat names (VarPat (span, v0)) =
        if mem (v0, names) then raise Fail "duplicate variable" else v0::names
    | globalNamesOfPat names (WildPat (span)) = []
    | globalNamesOfPat names (DotsPat (span)) = []
    | globalNamesOfPat names (IntPat (span, v0)) = []
    | globalNamesOfPat names (StrPat (span, v0)) = []
    | globalNamesOfPat names (ListPat (span, v0)) = globalNamesOfPat' names v0
  and globalNamesOfPat' names xs =
        List.foldr (fn (x, names) => globalNamesOfPat names x) names xs

  (* rename based on a given env *)
  fun rename (name, env) =
        case lookupEnv (name, env) of
             NONE => name (* unknown names are not renamed *)
           | SOME name' => name'

  (* extend env with names in a pattern *)
  fun extendPat (VarPat (span, v0), env) = extendEnv ((v0, gensym v0), env)
    | extendPat (WildPat span, env) = env
    | extendPat (DotsPat span, env) = env
    | extendPat (IntPat (span, v0), env) = env
    | extendPat (StrPat (span, v0), env) = env
    | extendPat (ListPat (span, v0), env) = List.foldr extendPat env v0

  (* a visitor that alpha-converts a program *)
  fun convertProgram env (Program (span, v0, v1)) =
        Program (span, convertTopLevel' env v0, convertNameTopLevel' env v1)
  and convertTopLevel' env xs = map (convertTopLevel env) xs
  and convertNameTopLevel' env xs = map (convertNameTopLevel env) xs
  and convertNameTopLevel env (NameTopLevel (span, v0, v1)) =
        NameTopLevel(span, convertLabel env v0, convertTopLevel' env v1)
  and convertLabel env (Label (span, v0)) = Label (span, v0)
  and convertTopLevel env (PatBody (span, v0, v1, v2)) =
        let
          val env' = List.foldr extendPat env v0
        in
          PatBody (span, convertPat' env' v0, convertGuard env' v1, convertBody env' v2)
        end
    | convertTopLevel env (GlobalVal (span, Val (span', v0, v1))) =
        (* keep patterns as are *)
        GlobalVal (span, Val (span', v0, convertLargeExp env v1))
    | convertTopLevel env (GlobalFun (span, Fun (span', v0, v1))) =
        (* keep function name as is *)
        GlobalFun (span, Fun (span', v0, convertFunBody' env v1))
  and convertGuard env (NoGuard (span)) = NoGuard span
    | convertGuard env (Guard (span, v0)) = Guard (span, convertLargeExp env v0)
  and convertFunBody env (FunBody (span, v0, v1)) =
        let
          val env' = List.foldr extendPat env v0
        in
          FunBody (span, convertPat' env' v0, convertLargeExp env' v1)
        end
  and convertFunBody' env xs = map (convertFunBody env) xs
  and convertPat env (VarPat (span, v0)) = VarPat (span, rename (v0, env))
    | convertPat env (WildPat (span)) = WildPat span
    | convertPat env (DotsPat (span)) = DotsPat span
    | convertPat env (IntPat (span, v0)) = IntPat (span, v0)
    | convertPat env (StrPat (span, v0)) = StrPat (span, v0)
    | convertPat env (ListPat (span, v0)) = ListPat (span, convertPat' env v0)
  and convertPat' env xs = map (convertPat env) xs
  and convertBody env (Body (span, v0)) = Body (span, convertStatement' env v0)
  and convertStatement' env [] = []
    | convertStatement' env (FunStatement (span, Fun (span', v0, v1))::xs) =
        let
          val name' = gensym v0
          val env' = extendEnv ((v0, name'), env)
          val x = FunStatement (span, Fun (span', name', convertFunBody' env' v1))
          val xs' = convertStatement' env' xs
        in
          x::xs'
        end
    | convertStatement' env (ValStatement (span, Val (span', v0, v1))::xs) =
        let
          val env' = List.foldr extendPat env v0
          val x = ValStatement (span, Val (span', convertPat' env' v0, convertLargeExp env v1))
          val xs' = convertStatement' env' xs
        in
          x::xs'
        end
    | convertStatement' env (NextStatement (span, v0, v1)::xs) =
        let
          val x = NextStatement (span, convertPat' env v0, convertLargeExp env v1)
          val xs' = convertStatement' env xs
        in
          x::xs'
        end
    | convertStatement' env (BangStatement (span, v0)::xs) =
        let
          val x = BangStatement (span, convertLargeExp env v0)
          val xs' = convertStatement' env xs
        in
          x::xs'
        end
    | convertStatement' env (BangStatement2 (span, v0, v1)::xs) =
        let
          val x = BangStatement2 (span, convertExp env v0 , convertExp' env v1)
          val xs' = convertStatement' env xs
        in
          x::xs'
        end
    | convertStatement' env (ForStatement (span, v0, v1, v2)::xs) =
        let
          val env' = List.foldr extendPat env v0
          val x = ForStatement (span, convertPat' env' v0, convertLargeExp env v1, convertStatement' env' v2)
          val xs' = convertStatement' env xs
        in
          x::xs'
        end
  and convertLargeExp env (PipeExp (span, v0, v1)) =
        PipeExp (span, convertLargeExp env v0, convertLargeExp env v1)
    | convertLargeExp env (AppExp (span, v0, v1)) =
        AppExp (span, convertLargeExp env v0, convertExp' env v1)
    | convertLargeExp env (Exp (span, v0)) = Exp (span, convertExp env v0)
  and convertExp env (FunExp (span, v0, v1)) =
        FunExp (span, convertPat' env v0, convertExp env v1)
    | convertExp env (EqExp (span, v0, v1)) =
        EqExp (span, convertExp env v0, convertExp env v1)
    | convertExp env (GtExp (span, v0, v1)) =
        GtExp (span, convertExp env v0, convertExp env v1)
    | convertExp env (LtExp (span, v0, v1)) =
        LtExp (span, convertExp env v0, convertExp env v1)
    | convertExp env (GeExp (span, v0, v1)) =
        GeExp (span, convertExp env v0, convertExp env v1)
    | convertExp env (LeExp (span, v0, v1)) =
        LeExp (span, convertExp env v0, convertExp env v1)
    | convertExp env (ConsExp (span, v0, v1)) =
        ConsExp (span, convertExp env v0, convertExp env v1)
    | convertExp env (AddExp (span, v0, v1)) =
        AddExp (span, convertExp env v0, convertExp env v1)
    | convertExp env (SubExp (span, v0, v1)) =
        SubExp (span, convertExp env v0, convertExp env v1)
    | convertExp env (MulExp (span, v0, v1)) =
        MulExp (span, convertExp env v0, convertExp env v1)
    | convertExp env (DivExp (span, v0, v1)) =
        DivExp (span, convertExp env v0, convertExp env v1)
    | convertExp env (App2Exp (span, v0, v1)) =
        App2Exp (span, convertExp env v0, convertExp' env v1)
    | convertExp env (App3Exp (span, v0)) = App3Exp (span, convertExp env v0)
    | convertExp env (VarExp (span, v0)) = VarExp (span, rename (v0, env))
    | convertExp env (IntExp (span, v0)) = IntExp (span, v0)
    | convertExp env (StrExp (span, v0)) = StrExp (span, v0)
    | convertExp env (DotsExp (span)) = DotsExp span
    | convertExp env (ListExp (span, v0)) = ListExp (span, convertExp' env v0)
    | convertExp env (NilExp (span)) = NilExp span
    | convertExp env (BlockExp (span, v0)) = BlockExp (span, convertStatement' env v0)
    | convertExp env (LargeExp (span, v0)) = LargeExp (span, convertLargeExp env v0)
  and convertExp' env xs = map (convertExp env) xs

  (* alpha-convert a program *)
  fun convert program =
        let
          val globals = globalNamesOfProgram [] program
          fun f (n, env) = extendEnv ((n, n), env)
          val env = List.foldr f initEnv globals
        in
          List.app (fn x => (print x; print "\n")) globals;
          convertProgram env program
        end
end
