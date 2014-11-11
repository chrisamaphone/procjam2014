structure SceneParse =
struct

fun startsWith exemplar str = 
let val n = size exemplar
in
   size str >= n andalso String.extract(str, 0, SOME n) = exemplar
end

val toks = String.tokens Char.isSpace

fun pkgScene cfg lines variants = 
let val variants = map (String.concatWith "\n" o rev) (rev (lines :: variants))
in
   (cfg, variants)
end

fun grabNewScene file config_line scenes = 
   case (toks config_line, map Int.fromString (toks config_line)) of 
      (["::", name, _], [_, _, SOME i]) => grabScene file [] [] (name, i) scenes
    | (["::", name], _) => grabScene file [] [] (name, 0) scenes
    | _ => raise Fail ("Config line \""^config_line^
                       "\" not of the form :: name or :: name n") 

and grabScene file lines variants cfg scenes =
   case TextIO.inputLine file of 
      NONE => rev (pkgScene cfg lines variants :: scenes)
    | SOME str =>
      if startsWith "||" str 
         then grabScene file lines (lines :: variants) cfg scenes
      else if startsWith "::" str 
         then grabNewScene file str (pkgScene cfg lines variants :: scenes)
      else grabScene file (str :: lines) variants cfg scenes

fun grab file =
   case TextIO.inputLine file of
      NONE => raise Fail ("File parsed, no configuration lines found!")
    | SOME str => 
      if startsWith "::" str
         then grabNewScene file str []
      else grab file

fun parseScenes fname: Scenes.scene list = 
let
   val file = TextIO.openIn fname
   val scenes = grab file before TextIO.closeIn file
in
   map (fn ((name, followable), multi_contents) => 
           {name = name, 
            followable = followable,
            contents = [Scenes.Text (hd multi_contents)]}) scenes
end

end
