structure Top:>
sig
   (* Takes:
       - the rough desired size of the file
       - the random seed
       - the input .out file produced by celf
       - the input .scenes file
       - the filename .tw of the twee file we're producing *)
   val go: int -> int -> string -> string -> string -> unit

   val world: string -> unit (* Takes world name, sets up call to go *)
end =
struct

fun startsWith str exemplar = 
let val n = size exemplar
in
   size str >= n andalso String.extract(str, 0, SOME n) = exemplar
end

fun grabTraces file accum = 
   case TextIO.inputLine file of 
      NONE => rev accum
    | SOME str => 
      if startsWith str "Solution"
         then grabTrace file accum [str] 
      else grabTraces file accum

and grabTrace file accum trace = 
   case TextIO.inputLine file of 
      NONE => rev (rev trace :: accum)
    | SOME str => 
      if startsWith str "    let "
         then grabTrace file accum (str :: trace)
      else grabTraces file (rev trace :: accum)

fun selectBest n best traces = 
   case traces of 
      [] => best
    | trace :: traces => 
      if abs (n - length trace) < abs (n - length best)
         then selectBest n trace traces
      else selectBest n best traces

fun go size seed infile scenefile outfile = 
let  
   val file = TextIO.openIn infile
   val traces = grabTraces file [] before TextIO.closeIn file
   val trace = String.concat (selectBest size (hd traces) (tl traces))
   val clf = Parser.parseString trace
   val scenes = SceneParse.parseScenes scenefile
   val twee = CLFtoTwee.compile scenes clf (Random.rand (0xc1fcafe, seed))
in
   Twee.printTwee outfile twee
end

fun world name = 
let
   fun adddir fname =
      OS.Path.joinDirFile 
          {dir = OS.Path.joinDirFile {dir = "world", file = name},
           file = fname}
   val infile = 
      adddir (OS.Path.joinBaseExt {base = name, ext = SOME "out"})
   val scenefile = 
      adddir (OS.Path.joinBaseExt {base = name, ext = SOME "scenes"})
   val outfile =
      adddir (OS.Path.joinBaseExt {base = name, ext = SOME "tw"})
in
   go 50 0xc1fface infile scenefile outfile
end

end
