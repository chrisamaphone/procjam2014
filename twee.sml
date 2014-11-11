structure Twee:>
sig
   (* Write the twee to to the given filename *)
   val printTwee: string -> ProtoTwee.twee -> unit
end = 
struct

open ProtoTwee

val simplebox_code = 
":: \"simple box\" css [stylesheet]\n\
\html {\n\
\  /* Vertical colour gradient */\n\
\  background-image: linear-gradient(to bottom,  gainsboro, silver);\n\
\  background-image: -webkit-linear-gradient(top,  gainsboro, silver);\n\
\  background-attachment: fixed;\n\
\\n\
\  /* Fallback colour */\n\
\  background-color: silver;\n\
\}\n\
\body {\n\
\  /* Remove default styles */\n\
\  background-color: transparent;\n\
\  margin: 10% 0 0 0;\n\
\  font-size: 100%;\n\
\  /* Used to center the box */\n\
\  text-align: center;\n\
\}\n\
\\n\
\#passages {\n\
\  /* Box background (white with 70% opacity) */\n\
\  background-color: rgba(255, 255, 255, 0.7);\n\
\\n\
\  /* Border */\n\
\  border: 2px solid white;\n\
\\n\
\  /* Rounded corners */\n\
\  border-radius: 1em;\n\
\\n\
\  /* Box width */\n\
\  width: 60%;\n\
\\n\
\  /* Center the box */\n\
\  display: inline-block;\n\
\  min-height: 40%;\n\
\  margin:auto;\n\
\  margin-bottom: 5%;\n\
\  padding: 0px;\n\
\}\n\
\\n\
\.passage {\n\
\  margin: 0px;\n\
\  /* Inner margin within the box */\n\
\  padding: 2em;\n\
\\n\
\  /* Text formatting */\n\
\  color: black;\n\
\  font-size: 100%;\n\
\  text-align:justify;\n\
\}\n\
\\n\
\/* No sidebar */\n\
\#sidebar {\n\
\  display:none;\n\
\}\n\
\\n\
\/* Links */\n\
\a.internalLink, a.externalLink {\n\
\  color: royalblue;\n\
\}\n\
\a.internalLink:hover, a.externalLink:hover {\n\
\  color: deepskyblue;\n\
\  text-decoration: none;\n\
\}\n\
\\n\
\/* Shrink the page when viewed on devices with a low screen width */\n\
\@media screen and (max-width: 960px) {\n\
\  .passage { font-size: 90%;}\n\
\  #passages { width: 70%; }\n\
\}\n\
\@media screen and (max-width: 840px) {\n\
\  .passage {  font-size: 87.5%; }\n\
\  #passages { width: 80%; }\n\
\}\n\
\@media screen and (max-width: 720px) {\n\
\  .passage { font-size: 75%; }\n\
\  #passages { width: 90%; }\n\
\}\n\
\"


fun printStyle p style =
   case style of 
      Default => ()
    | SimpleBox => p simplebox_code

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
   printStyle p (#style twee);
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
