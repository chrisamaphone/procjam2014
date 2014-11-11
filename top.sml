structure Top =
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

fun go size infile outfile = 
let  
   val file = TextIO.openIn infile
   val traces = grabTraces file [] before TextIO.closeIn file
   val trace = String.concat (selectBest size (hd traces) (tl traces))
   val clf = Parser.parseString trace
   val twee = CLFtoTwee.compile clf
in
    Twee.printTwee outfile twee
end

end
