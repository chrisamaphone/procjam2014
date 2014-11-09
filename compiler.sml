structure CLFtoTwee = struct

  (* util *) 
  fun member x = List.exists (fn x' => x = x')

  fun mapi' i f [] = []
    | mapi' i f (x::xs) = (f(x,i))::(mapi' (i+1) f xs)

  fun mapi f l = mapi' 0 f l


  (* hardcoded scene text for now *)
  fun sceneText (rulename, consts) =
    case rulename of
         "move" => List.nth(consts,0)
                  ^ " departs the " ^
                   List.nth(consts,1)
                  ^ " toward the " ^
                   List.nth(consts,2) ^ "."
      |  "pickup" => List.nth(consts,0)
                  ^ " picks up the "
                  ^ List.nth(consts,1)
                  ^ "."
      |  "drop"   => List.nth(consts,0)
                  ^ " drops the "
                  ^ List.nth(consts,1)
                  ^ "."
      |  "observe" => List.nth(consts,0)
                  ^ " notices the "
                  ^ List.nth(consts,1)
                  ^ "."
      |  "comment_on_location" => 
                    "\"It's so nice to be here in the "
                  ^ List.nth(consts,2) ^ ", don't you think, "
                  ^ List.nth(consts,1) ^ "?\" says "
                  ^ List.nth(consts,0) ^ "."
      | "greet" => "\"Hello, "^List.nth(consts,1)^",\" says "
                  ^ List.nth(consts,0) ^ "."
      | "observe_with" =>
                    "\"I see you have that "
                  ^ List.nth(consts,2) ^ ", "
                  ^ List.nth(consts,1) ^ ",\" says "
                  ^ List.nth(consts,0) ^ "."
      | "threaten_with_revolver" =>
                    List.nth(consts,0)
                  ^ " brandishes the revolver menacingly at "
                  ^ List.nth(consts,1)
                  ^ "."
      | "leave" => List.nth(consts,0) ^ " and " ^ List.nth(consts,1)
                  ^ " depart, arm in arm."
      | _ => rulename ^ " " ^ (String.concatWith " " consts)

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
        val outpassages = mapi mkOutPassage outputs
      in
        {name = rulepassage_name,
          contents = rulepassage_contents}
        ::(outpassages @ (compile_epsilon e))
      end
      handle (Error s) => (print s; raise Match)

  fun compile_initial inputs initial e =
  let
    (*  val name = "Start" *)
    val start_text = ProtoTwee.Text ("dummy start text")
    val displays = map display (List.take (initial, List.length inputs))
    val outpassages =  
      mapi (fn (c,i) => makeVarPassage e c (List.nth(initial,i))) inputs
  in
    (start_text::displays, outpassages)
  end

  (* XXX currently does not actually depend on the CelfTrace "final" field *)
  fun compile_final () =
    {name = "final", contents = [ProtoTwee.Text "final passage text"]}

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
     contents = initial_var_passages @ (compile_epsilon epsilon) @ ([compile_final ()])}
  end

end
