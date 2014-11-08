structure CLFtoTwee = struct

  fun member x = List.exists (fn x' => x = x')

  fun lookupConsumer e xi = 
  let
    fun xi_finder {rule, consts, inputs, outputs} =
      member xi inputs
  in
    List.find xi_finder e
  end

  exception Error of string

  fun makeVarPassage e xi =
  let
    val name = "X"^(Int.toString xi)
    val consumer = (* rule that consumes xi *)
      lookupConsumer e xi
  in
    case consumer of
         NONE => {name=name, contents=[ProtoTwee.Follow("Follow ?", "final")]}
          (* raise Error ("Couldn't find consumer of var "^name) *)
       | SOME {rule, consts, inputs, outputs} =>
          let
            val contents = ProtoTwee.Follow ("Follow ?",rule)
          in
            {name=name, contents=[contents]}
          end
  end

  fun display xi = ProtoTwee.Display ("X"^(Int.toString xi))

  fun compile_epsilon [] = []
    | compile_epsilon ({rule, consts, inputs, outputs}::e) =
      let
        val rulepassage_name = rule
        val displays =  map display outputs
        val scenetext = ProtoTwee.Text ("dummy scene text")
        val rulepassage_contents = scenetext::displays
        val outpassages = map (makeVarPassage e) outputs
      in
        {name = rulepassage_name,
          contents = rulepassage_contents}
        ::(outpassages @ (compile_epsilon e))
      end
      handle (Error s) => (print s; raise Match)

  fun compile_initial initial =
  let
    val name = "Start"
    val start_text = ProtoTwee.Text ("dummy start text")
    val displays = map display initial
  in
    {name=name, contents=start_text::displays}
  end

  (* XXX currently does not actually dependon the CelfTrace "final" field *)
  fun compile_final () =
    {name = "final", contents = [ProtoTwee.Text "final passage text"]}

  (* compile : CelfTrace.clftrace -> ProtoTwee.twee *)
  fun compile {initial, epsilon, final} =
    (compile_initial initial)::
    ((compile_epsilon epsilon) @ ([compile_final ()]))

end
