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

  (* hardcoded scene text for now *)

  val move_scene =
    {name = "move", followable = 1,
     contents = [ Scenes.Var 0, Scenes.Text " departs the ", Scenes.Var 1, Scenes.Text " toward the ",
                  Scenes.Var 2, Scenes.Text "." ]}

  val pickup_scene =
    {name = "pickup", followable = 2,
     contents = [ Scenes.Var 0, Scenes.Text " picks up the ", Scenes.Var 1, Scenes.Text "." ]}

  val drop_scene =
    {name = "drop", followable = 2,
     contents = [ Scenes.Var 0, Scenes.Text " drops the ", Scenes.Var 1, Scenes.Text "."] }

  val observe_scene =
    {name = "observe", followable = 2,
      contents = [ Scenes.Var 0, Scenes.Text " notices the ", Scenes.Var 1, Scenes.Text "."] }

  val comment_on_location_scene =
    {name = "comment_on_location", followable = 2,
      contents = [ Scenes.Text "\"It's so nice to be in the ", Scenes.Var 2,
                   Scenes.Text ", don't you think, ", Scenes.Var 1, Scenes.Text "?\" says ",
                   Scenes.Var 0, Scenes.Text "." ] }

  val greet_scene =
    {name = "greet", followable = 2,
      contents = [ Scenes.Text "\"Hello, ", Scenes.Var 1, Scenes.Text ",\" says ", Scenes.Var 0,
                   Scenes.Text "." ]}

  val observe_with_scene =
    {name = "observe_with", followable = 3,
      contents = [ Scenes.Text "\"I see you have that ", Scenes.Var 2, Scenes.Text ", ", Scenes.Var 1,
                   Scenes.Text ",\" says ", Scenes.Var 0, Scenes.Text "." ]}

  val threaten_with_revolver_scene =
    {name = "threaten_with_revolver", followable = 2,
      contents = [ Scenes.Var 0, Scenes.Text " brandishes the revolver menacingly at ",
                   Scenes.Var 1, Scenes.Text "." ]}

  val leave_scene =
    {name = "leave", followable = 0,
      contents = [ Scenes.Var 0, Scenes.Text " and ", Scenes.Var 1, Scenes.Text " depart, arm in arm." ]}


  val scenes =
    [move_scene, pickup_scene, drop_scene, observe_scene,
    comment_on_location_scene, greet_scene, observe_with_scene,
    threaten_with_revolver_scene, leave_scene]


  fun sceneText (rulename, consts) =
  let
    val scene = List.find (fn {name, ...} => name = rulename) scenes
  in
    case scene of 
        NONE => rulename ^ (String.concatWith " " consts)
      | SOME {name, followable, contents} =>
        let
          fun stringifyComponent c = (case c of
                (Scenes.Text t) => t
              | (Scenes.Var i) => List.nth (consts, i))
          val stringComponents = map stringifyComponent contents
        in
          String.concat stringComponents
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


  fun compile_epsilon [] = []
    | compile_epsilon ({rule, consts, inputs, outputs}::e) =
      let
        val rulepassage_name = passageName rule inputs
        val displays =  map display outputs
        val scenetext = ProtoTwee.Text (sceneText (rule, consts))
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
        ::(outpassages @ (compile_epsilon e))
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

  (* compile : CelfTrace.clftrace -> ProtoTwee.twee *)
  fun compile {consts, initial, epsilon, final}: ProtoTwee.twee =
  let
    val (initial_passage, initial_var_passages) = 
      compile_initial consts initial epsilon
  in
    {start = initial_passage,
     style = ProtoTwee.Default,
     title = "Performed for you",
     author = "Celf Sparrow",
     contents = initial_var_passages 
              @ (compile_epsilon epsilon) 
              @ ([compile_final final])}
  end

end
