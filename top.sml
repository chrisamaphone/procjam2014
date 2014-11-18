structure Top:>
sig
   (* Takes:
       - the rough desired size of the file
       - the random seed
       - the input .out file produced by celf
       - the input .scenes file
       - optional stats file 
       - the filename .tw of the twee file we're producing *)
   val go: int -> int -> string -> string -> string option -> string -> unit

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

fun getTwee size seed infile scenefile =
let  
   val file = TextIO.openIn infile
   val traces = grabTraces file [] before TextIO.closeIn file
   val () = case traces of 
               [] => raise Fail "No traces in CLF's output"
             | _ => ()
   val trace = String.concat (selectBest size (hd traces) (tl traces))
   val clf = Parser.parseString trace 
   val trace = CelfTrace.clf_to_trace clf
   val scenes = SceneParse.parseScenes scenefile
in
  CLFtoTwee.compile scenes trace (Random.rand (0xc1fcafe, seed))
end


fun go size seed infile scenefile statsfile outfile = 
let  
   val file = TextIO.openIn infile
   val traces = grabTraces file [] before TextIO.closeIn file
   val () = case traces of 
               [] => raise Fail "No traces in CLF's output"
             | _ => ()
   val trace = String.concat (selectBest size (hd traces) (tl traces))
   val clf = Parser.parseString trace 
   val trace = CelfTrace.clf_to_trace clf
   val scenes = SceneParse.parseScenes scenefile
   val twee = CLFtoTwee.compile scenes trace (Random.rand (0xc1fcafe, seed))

   val allPaths = StoryStats.allPaths twee
   val statEpsilonLength = length (#epsilon trace)
   val statAverageLength = StoryStats.averageLength allPaths
   val statLongestPath = StoryStats.longestPath allPaths
   val () = case statsfile of
               NONE => ()
             | SOME fname => 
               let 
                  val f = TextIO.openOut fname
               in
                  TextIO.output (f, Int.toString statEpsilonLength^"\n");
                  TextIO.output (f, Real.toString statAverageLength^"\n");
                  (* TextIO.output (f, Int.toString statLongestPath^"\n"); *)
                  TextIO.closeOut f
               end
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
   val statfile = 
      adddir (OS.Path.joinBaseExt {base = name, ext = SOME "stats"})
in
   go 5000 0xc1fface infile scenefile (SOME statfile) outfile
end

end
