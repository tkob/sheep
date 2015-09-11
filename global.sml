structure Global = struct
  open Parse.Ast

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

  fun globalNames program = globalNamesOfProgram [] program
end
