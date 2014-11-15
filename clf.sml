structure CLF = 
struct

type resource_var = int

datatype mod = Lin | Aff | Pers

datatype pattern = 
  POne
| PXid of mod * resource_var
| PId of string
| PPair of pattern * pattern

datatype value = 
  One
| Down of mod * exp
| Pair of value * value

and exp = 
  Xid of resource_var * value list
| Id of string * value list 
| Lam of pattern * exp
| Lax of mexp

and mexp = 
  Let of pattern * exp * mexp
| In of value

end
