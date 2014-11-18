structure SceneParse:>
sig
   (* Takes filename, parses contents *)
   val parseScenes : string -> Scenes.scene list
end =
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
         then grabScene file [] (lines :: variants) cfg scenes
      else if startsWith "::" str 
         then grabNewScene file str (pkgScene cfg lines variants :: scenes)
      else if startsWith "%" str
         then grab file (pkgScene cfg lines variants :: scenes)
      else grabScene file (str :: lines) variants cfg scenes

and grab file scenes =
   case TextIO.inputLine file of
      NONE => raise Fail ("File parsed, no configuration lines found!")
    | SOME str => 
      if startsWith "::" str
         then grabNewScene file str scenes
      else grab file scenes

fun pkgText text = Scenes.Text (implode (rev text))

fun munchText name str text components =
   case str of 
      [] => rev (pkgText text :: components)
    | #"<" :: str => munchVar name str [] text components
    | c :: str => munchText name str (c :: text) components

and munchVar name str num text components = 
   case str of
      [] => 
      raise Fail ("Passage "^name^" ended with unclosed tag <"^
                  implode (rev str))
    | #"<" :: str => 
      raise Fail ("In passage "^name^", nested tags <"^
                  implode (rev (#"<" :: str)))
    | #">" :: str => 
      (case Int.fromString (implode (rev num)) of 
          NONE => munchText name str 
                     ((#">" :: num) @ (#"<" :: text)) components
        | SOME i => munchText name str [] (* XXX bounds-check i *)
                       (Scenes.Var (i-1) :: pkgText text :: components))
    | c :: str => munchVar name str (c :: num) text components

fun munch name variants = 
let
   val variants = 
      map (Substring.string o 
           Substring.dropl Char.isSpace o 
           Substring.dropr Char.isSpace o 
           Substring.full) 
          variants
   fun mapper variant = munchText name (String.explode variant) [] []
   val variants_parsed = map mapper variants
in
   map mapper variants
end

fun parseScenes fname: Scenes.scene list = 
let
   val file = TextIO.openIn fname
   val scenes = grab file [] before TextIO.closeIn file
in
   map (fn ((name, followable), variants) => 
           {name = name, 
            followable = followable,
            contents = munch name variants}) scenes
end

end
