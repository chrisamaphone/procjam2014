(* Note: if the justification of a scene involves anything besides a
 * resource (like a proof that the revolver is portable), we ignore it. *)

structure CelfTrace = 
struct

type resource_var = int
type step = {rule: string, 
             consts: string list, 
             inputs: resource_var list,
             outputs: resource_var list}
type clftrace = {initial: (string * resource_var) list,
                 epsilon: step list,
                 final: resource_var list}

end

structure Script = 
struct

type var = int
datatype component = Var of var | Text of string
type scene = {name: string,
              followable: int,
              contents: component list}

end

structure ProtoTwee = 
struct

datatype component = Text of string 
                   | Display of string
                   | Follow of string * string

type passage = {name: string,
                contents: component list}

datatype style = Default | SimpleBox

type twee = {start: component list,
             style: style,
             title: string,
             author: string,
             contents: passage list}

end
