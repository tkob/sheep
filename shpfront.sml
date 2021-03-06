structure TypeUtils = struct
  fun isEntier s =
        let
          val dropMinus = case explode s of #"-"::cs => cs | cs => cs
        in
          List.all Char.isDigit dropMinus
        end
  fun isDouble s =
        case Real.fromString s of
             NONE => false
           | SOME _ =>
               not (Substring.isSubstring "~" (Substring.full s))
  fun isTrue "TRUE" = true
    | isTrue "True" = true
    | isTrue "true" = true
    | isTrue _      = false
  fun isFalse "FALSE" = true
    | isFalse "False" = true
    | isFalse "false" = true
    | isFalse _       = false
end

structure EncodingIO = struct
  fun execForOutput args =
        let
          fun mkInstream fd =
                let
                  val reader = Posix.IO.mkTextReader
                      {fd = fd, name = "", initBlkMode = true}
                  val instream = TextIO.StreamIO.mkInstream (reader, "")
                in
                  instream
                end
          val c2p = Posix.IO.pipe ()
        in
          case Posix.Process.fork () of
               NONE => (* child *)
                 let
                   val cmd = hd args
                 in
                   Posix.IO.dup2 {old = #outfd c2p, new = Posix.FileSys.stdout};
                   Posix.IO.close (#infd c2p);
                   Posix.Process.execp (cmd, args)
                 end
             | SOME pid => (* parent *)
                 let
                   val ins = mkInstream (#infd c2p)
                 in
                   Posix.IO.close (#outfd c2p);
                   ins
                 end
        end

  fun openIn (fileName, encoding) =
        let
          val encodingOpt = case encoding of
                                 NONE => []
                               | SOME name => ["-encoding", name]
          val args = "shpconv.tcl" :: encodingOpt @ [fileName]
          val ins = execForOutput args
        in
          ins
        end

  fun getStdIn encoding =
        let
          val encodingOpt = case encoding of
                                 NONE => []
                               | SOME name => ["-encoding", name]
          val args = "shpconv.tcl" :: encodingOpt
          val ins = execForOutput args
        in
          ins
        end

end

structure AwkReader = struct
  open TypeUtils
  open MessagePackBinIO.Pack

  local
    val stdoutWriter = Posix.IO.mkBinWriter {
      fd = Posix.FileSys.stdout,
      name = "stdout",
      appendMode = true,
      initBlkMode = false,
      chunkSize = 1024 }
  in
    val stdout = BinIO.StreamIO.mkOutstream (stdoutWriter, IO.BLOCK_BUF)
  end

  fun appHeadAndTail fHead fTail [] = ()
    | appHeadAndTail fHead fTail (x::xs) = (
        fHead x;
        List.app fTail xs)

  fun escapeAndPrint s =
        let
          val s = Substring.full s
          fun p #"\"" = print "\\\""
            | p c = TextIO.output1 (TextIO.stdOut, c)
        in
          Substring.app p s
        end
  fun awkRead ins =
        let
          fun println s = (print s; print "\n")
          val scan = CSV.scanAwk TextIO.StreamIO.input1
          fun loop ins =
                case scan ins of
                     NONE => ()
                   | SOME (record, ins) =>
                       let
                         fun packField field outs =
                               if isEntier field then
                                 doPack packInt (Option.valOf (Int.fromString field)) outs
                               else if isDouble field then
                                 doPack packReal (Option.valOf (Real.fromString field)) outs
                               else if isTrue field then
                                 doPack packBool true outs
                               else if isFalse field then
                                 doPack packBool false outs
                               else
                                 doPack packString field outs
                       in
                         doPack (packList (fromFn packField)) record stdout;
                         loop ins
                       end
        in
          loop ins
        end

  fun csvRead ins =
        let
          fun println s = (print s; print "\n")
          val scan = CSV.scanCSV TextIO.StreamIO.input1
          fun loop ins =
                case scan ins of
                     NONE => ()
                   | SOME (record, ins) =>
                       let
                         fun emitField field =
                               if isEntier field then
                                 print field
                               else if isDouble field then
                                 print field
                               else if isTrue field then
                                 print "true"
                               else if isFalse field then
                                 print "false"
                               else (
                                 print "{%str \"";
                                 escapeAndPrint field;
                                 print "\"}")
                         fun emitSpaceAndField field = (
                               print " ";
                               emitField field)
                       in
                         appHeadAndTail emitField emitSpaceAndField record;
                         print "\n";
                         loop ins
                       end
        in
          loop ins
        end
end

structure SheepFront = struct
  datatype reader = TextStreamReader of TextIO.StreamIO.instream -> unit
                  | FileReader of string -> unit
  val readers = [
    ("awk", TextStreamReader AwkReader.awkRead),
    ("csv", TextStreamReader AwkReader.csvRead)
  ]

  fun fail msg = raise Fail msg
  fun splitBy delim s =
        let
          val (first, rest) = Substring.splitl (fn c => c <> delim) s
        in
          (Substring.string first, Substring.string (Substring.triml 1 rest))
        end
  fun parseOpts opts =
        let
          val opts = Substring.full opts
          fun parseFormat s =
                let
                  val (format, s') = Substring.splitl Char.isAlpha s
                in
                  if Substring.sub (s', 0) = #":" then
                    (Substring.string format, Substring.triml 1 s')
                  else
                    ("awk", s)
                end
          val (format, opts) = parseFormat opts
          val pairs = Substring.tokens (fn c => c = #",") opts
          val keyValues = map (splitBy #"=") pairs
        in
          (format, keyValues)
        end
  fun lookup key [] = NONE
    | lookup key ((key', value)::keyValues) =
        if key = key' then SOME value
        else lookup key keyValues
  fun parseOptions args =
        let
          fun parse (options, "-opts"::args) =
                (case args of
                      [] => fail "-opts requires an argument"
                    | opts::args =>
                        let
                          val (format, opts) = parseOpts opts
                          val options = {format = format, options = opts}
                        in
                          parse (options, args)
                        end)
            | parse (options, "--"::args) =
                (options, args)
            | parse (options, arg::args) =
                if String.isPrefix "-"  arg
                then fail ("unknown option: " ^ arg)
                else (options, arg::args)
            | parse (options, []) =
                (options, [])
        in
          parse ({format = "awk", options = []}, args)
        end

  fun main (_, arguments) =
        let
          val ({format, options}, args) = parseOptions arguments
          val reader = case lookup format readers of
                            SOME reader => reader
                          | NONE => fail ("unknown format: " ^ format)
        in
          if length args > 0 then
            let
              fun f fileName =
                    case reader of
                         TextStreamReader reader =>
                           let
                             val ins = EncodingIO.openIn (fileName, lookup "encoding" options)
                             fun release () = TextIO.StreamIO.closeIn ins
                           in
                             (reader ins; release ())
                             handle e => (release (); raise e)
                           end
                       | FileReader reader => reader fileName
            in
              List.app f args
            end
          else
            case reader of
                 TextStreamReader reader =>
                   let
                     val ins = EncodingIO.getStdIn (lookup "encoding" options)
                     fun release () = TextIO.StreamIO.closeIn ins
                   in
                     (reader ins; release ())
                     handle e => (release (); raise e)
                   end
               | FileReader reader => fail ("cannot read stdin for " ^ format);
          OS.Process.success
        end

end

fun main () =
  let
    val name = CommandLine.name ()
    val arguments = CommandLine.arguments ()
  in
      OS.Process.exit (SheepFront.main (name, arguments))
  end
