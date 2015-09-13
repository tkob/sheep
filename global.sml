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
  and globalNamesOfTopLevel names (PatBody (span, v0, v1, v2)) = names
    | globalNamesOfTopLevel names (Begin (span, v0)) = names
    | globalNamesOfTopLevel names (End (span, v0)) = names
    | globalNamesOfTopLevel names (GlobalVal (span, v0)) =
        globalNamesOfValDef names v0
    | globalNamesOfTopLevel names (GlobalFun (span, v0)) =
        globalNamesOfFunDef names v0
  and globalNamesOfValDef names (Val (span, v0, v1)) =
      globalNamesOfPat' names v0
  and globalNamesOfFunDef (vals, funs) (Fun (span, name, _)) =
        if mem (name, vals) orelse mem (name, funs) then
          raise Fail "duplicate variable"
        else (vals, name::funs)
  and globalNamesOfPat (vals, funs) (VarPat (span, name)) =
        if mem (name, vals) orelse mem (name, funs) then
          raise Fail "duplicate variable"
        else (name::vals, funs)
    | globalNamesOfPat names (WildPat (span)) = names
    | globalNamesOfPat names (DotsPat (span)) = names
    | globalNamesOfPat names (IntPat (span, v0)) = names
    | globalNamesOfPat names (StrPat (span, v0)) = names
    | globalNamesOfPat names (ListPat (span, v0)) = globalNamesOfPat' names v0
  and globalNamesOfPat' names xs =
        List.foldr (fn (x, names) => globalNamesOfPat names x) names xs

  fun globalNames program = globalNamesOfProgram ([], []) program
end
