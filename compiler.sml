structure CLFtoTwee = struct

  (* util *) 
  fun member x = List.exists (fn x' => x = x')

  fun mapi' i f [] = []
    | mapi' i f (x::xs) = (f(x,i))::(mapi' (i+1) f xs)

  fun mapi f l = mapi' 0 f l

  fun truncate [] n = []
    | truncate l 0 = []
    | truncate (x::xs) n = x::(truncate xs (n-1))

  fun truncateMapi f l n = mapi f (truncate l n)

  fun randElt l r = List.nth (l, Random.randRange (0, List.length l - 1) r)

  (* returns (text for the scene, # of followable links) *)
  fun sceneText scenes (rulename, consts) rand =
  let
    val scene = List.find (fn {name, ...} => name = rulename) scenes
  in
    case scene of 
        NONE => (rulename ^ " " ^ (String.concatWith " " consts), 99999)
          before print ("\n\nWarning: scene file has no scene for rule "
                        ^ rulename ^"\n\n")
      | SOME {name, followable, contents=content_variants} =>
        let
          val contents = randElt content_variants rand
          fun stringifyComponent c = (case c of
                (Scenes.Text t) => t
              | (Scenes.Var i) => List.nth (consts, i)
                  handle Subscript => 
                    (print ("\n\nProblem with scene "^name
                      ^ ": numeric reference exceeds number of Pi-bound vars in rule\n\n");
                    raise Match))
          val stringComponents = map stringifyComponent contents
        in
          (String.concat stringComponents, followable)
        end
  end

  (***)

  fun lookupConsumer e xi = 
  let
    fun xi_finder {rule, consts, inputs, outputs} =
      member xi inputs
  in
    List.find xi_finder e
  end

  fun passageName rule inputs = rule^Int.toString(hd inputs)

  exception Error of string

  fun makeVarPassage e linkText xi  =
  let
    val name = "X"^(Int.toString xi)
    val consumer = (* rule that consumes xi *)
      lookupConsumer e xi
  in
    case consumer of
         NONE => {name=name, contents=[ProtoTwee.Follow(linkText, "final")]}
          (* raise Error ("Couldn't find consumer of var "^name) *)
       | SOME {rule, consts, inputs, outputs} =>
          let
            val contents = 
              ProtoTwee.Follow (linkText, passageName rule inputs)
          in
            {name=name, contents=[contents]}
          end
  end

  fun display xi = ProtoTwee.Display ("X"^(Int.toString xi))


  fun compile_epsilon scenes [] _ = []
    | compile_epsilon scenes ({rule, consts, inputs, outputs}::e) rand =
      let
        val rulepassage_name = passageName rule inputs
        val (scenetext, follow:int) = sceneText scenes (rule, consts) rand
        val scenetext = ProtoTwee.Text scenetext
        val displays =  map display (truncate outputs follow)
        val rulepassage_contents = scenetext::displays
        fun mkOutPassage (x, i) = makeVarPassage e (List.nth (consts,i)) x
          handle (Error s) => 
          (print ("was trying to compile rule "^rulepassage_name^"\n"); 
          print s; 
          raise Match)
        val outpassages = truncateMapi mkOutPassage outputs (List.length consts)
      in
        {name = rulepassage_name,
          contents = rulepassage_contents}
        ::(outpassages @ (compile_epsilon scenes e rand))
      end

  fun inputsToString [] _ = "no one and nothing."
    | inputsToString [noun] true = "and "^noun^"."
    | inputsToString [noun] false = noun ^ "."
    | inputsToString (noun::nouns) flag =
        noun ^ (if flag then ", " else " ") ^ (inputsToString nouns true) 
      
  fun inputsToString' l = "The world contains "^(inputsToString l false)

  fun compile_initial inputs initial e =
  let
    (*  val name = "Start" *)
    val start_text = ProtoTwee.Text (inputsToString' inputs)
    val displays = map display (List.take (initial, List.length inputs))
    val outpassages =  
      truncateMapi 
        (fn (c,i) => makeVarPassage e c (List.nth(initial,i))) 
        inputs (List.length initial)
  in
    (start_text::displays, outpassages)
  end

  (* XXX currently does not actually usefully refer to the CelfTrace "final" field *)
  fun compile_final final =
  let
    val text = (Int.toString (List.length final)) ^ 
          " resources were involved in the telling of this story. The End."
  in
    {name = "final", contents = [ProtoTwee.Text text]}
  end

  (* compile : Scenes.scene list -> CelfTrace.clftrace -> ProtoTwee.twee *)
  fun compile scenes {consts, initial, epsilon, final} rand : ProtoTwee.twee =
  let
    val (initial_passage, initial_var_passages) = 
      compile_initial consts initial epsilon
  in
    {start = initial_passage,
     style = ProtoTwee.SimpleBox,
     title = ("Performance #"^Int.toString (Random.randRange (0, 1000000) rand)),
     author = "Procjam 2014 Autocompiler",
     contents = initial_var_passages 
              @ (compile_epsilon scenes epsilon rand) 
              @ ([compile_final final])}
  end

end
