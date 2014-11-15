structure CelfTrace = 
struct

type resource_var = int
type step = {rule: string, 
             consts: string list, 
             inputs: resource_var list,
             outputs: resource_var list}
type clftrace = {consts: string list,
                 initial: resource_var list,
                 epsilon: step list,
                 final: resource_var list}

fun manip s = 
let
   val parts = String.fields (fn c => c = #"'")
                 (String.concatWith " " 
                    (String.fields (fn c => c = #"_") s))
   fun capsify s =
      if s = "" 
         then "" 
      else str (Char.toUpper (String.sub (s, 0))) ^ String.extract (s, 1, NONE)

   val () = 
      case parts of 
         [] => raise Fail "Manipulating a id that was apparently just \"'\""   
       | _ => ()
in
   String.concat (hd parts :: map capsify (tl parts))
end

fun pretty_const s =
let
   val parts = String.fields (fn c => c = #"'")
                 (String.concatWith " "
                    (String.fields (fn c => c = #"_") s))
   fun capsify s =
      if s = ""
         then ""
      else str (Char.toUpper (String.sub (s, 0))) ^ String.extract (s, 1, NONE)
   val () =
      case parts of
         [] => raise Fail "Manipulating a id that was apparently just \"'\""
       | _ => ()
in
   String.concat (hd parts :: map capsify (tl parts))
end

fun strip_consts exp consts =
   case exp of 
      CLF.Lam (CLF.PId c, exp) => strip_consts exp (pretty_const c :: consts)
    | CLF.Lam (init, CLF.Lax mexp) => (rev consts, init, mexp) 
    | _ => raise Fail "Initial expression ill-formed (strip_constants)"

fun read_pattern pattern =
   case pattern of 
      CLF.POne => []
    | CLF.PXid (_, x) => [x]
    | CLF.PId s => []
    | CLF.PPair (p1, p2) => read_pattern p1 @ read_pattern p2

fun read_inputs value = 
   case value of 
      CLF.Down (_, CLF.Xid (x, [])) => [x]
    | CLF.Pair (value1, value2) => read_inputs value1 @ read_inputs value2 
    | _ => [] 

fun read_scene rule values consts = 
   case values of 
      [] => raise Fail ("Rule arguments for '"^rule^
                        "' ill-formed (strip_scene_consts)")
    | CLF.Down (CLF.Pers, (CLF.Id (c, []))) :: values =>
      read_scene rule values (pretty_const c :: consts)
    | [value] => (rev consts, read_inputs value)
    | _ => raise Fail ("Rule arguments for '"^rule^
                       "' ill-formed (strip_scene_consts)")
                            
fun strip_epsilon mexp steps = 
   case mexp of 
      CLF.Let (pattern, CLF.Id (rule, values), mexp) => 
      let
         val outputs = read_pattern pattern
         val (consts, inputs) = read_scene rule values []
         val step = {rule = rule, 
                     consts = consts, 
                     inputs = inputs, 
                     outputs = outputs}
      in
         strip_epsilon mexp (step :: steps)
      end
    | CLF.In value => (rev steps, value)
    | _ => raise Fail ("Ill-formed sequence (strip_epsilon)") 

fun clf_to_trace exp = 
let 
   val (consts, pattern, mexp) = strip_consts exp []
   val initial = read_pattern pattern 
   val (epsilon, value) = strip_epsilon mexp []
   val final = read_inputs value
in
   {consts = consts, initial = initial, epsilon = epsilon, final = final}
end

end
