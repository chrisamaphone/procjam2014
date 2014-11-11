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

fun pkgText text = Scenes.Text (implode (rev text))

fun pkgVar name num = 
let
   val s = implode (rev num)
   val i = case Int.fromString s of
              NONE => raise Fail ("In passage "^name^", \"#"^
                                  s^"\" is not a valid identifier")
            | SOME i => 
              if 1 <= i then  i 
              else raise Fail ("Variable must be constant")
in
   Scenes.Var i
end

fun munchText name str text components =
   case str of 
      [] => rev (pkgText text :: components)
    | #"#" :: str => munchVar name str [] (pkgText text :: components)
    | c :: str => munchText name str (c :: text) components

and munchVar name str num components = 
   case str of
      [] => rev (pkgVar name num :: components)
    | c :: str =>
      if Char.isDigit c
         then munchVar name str (c :: num) components
      else munchText name str [] (pkgVar name num :: components)

fun munch name variants = 
let
   fun mapper variant = munchText name (String.explode variant) [] []
   val variants_parsed = map mapper variants
in
   hd (map mapper variants)
   (* XXX Chris remove the 'hd' part when you fix type *)
end

fun parseScenes fname: Scenes.scene list = 
let
   val file = TextIO.openIn fname
   val scenes = grab file before TextIO.closeIn file
in
   map (fn ((name, followable), variants) => 
           {name = name, 
            followable = followable,
            contents = munch name variants}) scenes
end

end
