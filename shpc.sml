structure SheepCompiler = struct
  fun main (_, arguments) =
       let
         val fileName = case arguments of [] => NONE | a::_ => SOME a
         val ins = case fileName of
                        NONE => TextIO.stdIn
                      | SOME name => TextIO.openIn name
         fun release () =
               if Option.isSome fileName then TextIO.closeIn ins else ()
       in
         let
           val strm = Lexer.streamifyInstream ins
           val sourcemap = case fileName of
                                NONE => AntlrStreamPos.mkSourcemap ()
                              | SOME n => AntlrStreamPos.mkSourcemap' n
           val trees = Parse.parse sourcemap strm
           val numParses = length trees
           (* val _ = print (Int.toString numParses ^ " parse(s)\n") *)
           val tree = hd trees
           val (globalVals, globalFuns) = Global.globalNames tree
           val alpha = Alpha.convert (tree, globalVals, globalFuns)
           val (topFuns, closure) =
             Closure.convert (alpha, globalVals, globalFuns)
         in
           GenTal.compile (topFuns, closure, globalVals, globalFuns, sourcemap);
           release ();
           OS.Process.success
         end
         handle e => (release (); raise e)
       end
end

fun main () =
  let
    val name = CommandLine.name ()
    val arguments = CommandLine.arguments ()
  in
      OS.Process.exit (SheepCompiler.main (name, arguments))
  end
