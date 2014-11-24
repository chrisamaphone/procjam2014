structure StoryStats =
struct

  fun getPassageFromStory passage_name story = 
    valOf (List.find (fn {name,contents} => name = passage_name) story)

  fun isBranch (ProtoTwee.Display _) = true
    | isBranch (ProtoTwee.Follow _) = true
    | isBranch _ = false

  fun pathsFromPassage story {name, contents} =
  let
    (* DEBUG  *)
    val () = print ("In passage "^name^"\n")
    val branches = List.filter isBranch contents
    val nbstring = Int.toString (List.length branches)
    val () = print ("Number of branches: "^nbstring^"\n")
    (* END DEBUG *)
    val paths1 = List.concat (map (pathsThroughComponent story name) contents)
    val paths = List.filter (fn l => not (List.null l)) paths1
  in
    case paths of
         [] => [[name]]  before print "\nLEAF\n\n" 
       | _ => map (fn path => name::path) paths
  end

  and pathsThroughComponent story parentname c =
    case c of 
          ProtoTwee.Text _ => []
        | ProtoTwee.Display name =>
            let
              val {name, contents} = getPassageFromStory name story
            in
              List.concat (map (pathsThroughComponent story parentname) contents)
            end
        | ProtoTwee.Follow (link, name) =>
            (
            print ("following "^link^" to "^name^" from "^parentname^"\n");  
            let
              val paths = pathsFromPassage story (getPassageFromStory name story)
            in
              paths
            end
            )


  (* generate all paths from start passage to passages w/no follow links *)
  fun allPaths {start,style,title,author,contents} = 
  let
    fun mapper c =
      let
        val paths = List.concat (pathsThroughComponent contents "start"  c)
      in
        map (fn path => "start"::paths) paths
      end
  in
    List.concat (map mapper start)
  end

  fun averageLength paths =
  let
    val lengths = map List.length paths
    val sum = foldl (op+) 0 lengths
    fun r i = Real.fromInt i
  in
    (r sum) / (r (List.length paths))
  end

  fun longestPath paths =
  let
    val pathsWithLength = map (fn p => (List.length p, p)) paths
  in
    foldl (fn ((n,p),(max,maxp)) => if n > max then (n,p) else (max, maxp)) 
      (0, []) pathsWithLength
  end

end

