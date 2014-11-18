
val usage = "Usage: tamaraify infile.out scenefile.scenes outfile.tw seed len\n"

val () = 
   case CommandLine.arguments () of 
      [infile, scenefile, outfile, seed, n] =>
      (case (Int.fromString seed, Int.fromString n) of
          (SOME seed, SOME n) => 
          (Top.go n seed infile scenefile NONE outfile;
           OS.Process.exit OS.Process.success)
        | _ => (print usage;
                   print "seed or len not an integer\n";
                   OS.Process.exit OS.Process.failure))
    | _ => (print usage;
            OS.Process.exit OS.Process.failure)
