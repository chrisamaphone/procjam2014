
val () = 
   case CommandLine.arguments () of 
      [infile, outfile, n] =>
      (case Int.fromString n of
          NONE => (print "Usage: tamaraify infile.out outfile.tw len\n";
                   print "len was not an integer\n";
                   OS.Process.exit OS.Process.failure)
        | SOME i => (Top.go i infile outfile;
                     OS.Process.exit OS.Process.success))
    | _ => (print "Usage: tamaraify infile.out outfile.tw len\n";
            OS.Process.exit OS.Process.failure)
