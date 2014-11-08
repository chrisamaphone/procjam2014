structure Twee = 
struct

open ProtoTwee

fun printStyle p style =
   case style of 
      Default => ()
    | SimpleBox => raise Fail "Not implemented"

fun printComponent p com = 
   case com of
      Text s => p s
    | Display s => p ("<<display \""^s^"\">>") 
    | Follow (name, link) => p ("[[Follow "^name^".|"^link^"]]")

fun printPassage p ({name, contents}: passage) = 
let
in
   p (":: "^name);
   app (printComponent p) contents;
   p ""
end

fun printTwee outfile (twee: twee) =
let
   val out = TextIO.openOut outfile
   fun pr f s = (TextIO.output (f, s); TextIO.output (f, "\n"))
   val p = pr out
in
   p (":: Start");
   app (printComponent p) (#start twee);
   p "";
   printStyle outfile (#style twee);
   p ":: StoryTitle";
   p (#title twee);
   p "";
   p ":: StoryAuthor";
   p (#author twee);
   p "";
   app (printPassage p) (#contents twee);
   TextIO.closeOut out
end



end
