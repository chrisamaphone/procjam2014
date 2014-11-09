structure Tokens = Tokens
type pos = unit
type svalue = Tokens.svalue
type lexresult = (svalue, pos) Tokens.token
val eof = fn () => Tokens.EOF ((), ())
type ('a, 'b) token = ('a, 'b) Tokens.token

fun id s = 
let
   val parts = String.fields (fn c => c = #"'")
                 (String.concatWith " " 
                    (String.fields (fn c => c = #"_") s))
   fun capsify s =
      if s = "" 
         then "" 
      else str (Char.toUpper (String.sub (s, 0))) ^ String.extract (s, 1, NONE)

in
   String.concat (hd parts :: map capsify (tl parts))
end

fun mkid str = 
   if (String.sub (str, 0) = #"X")
      then (case Int.fromString (String.extract (str, 1, NONE)) of
               NONE => Tokens.ID (id str, (), ())
             | SOME i => Tokens.XID (i, (), ()))
   else Tokens.ID (id str, (), ())

%%

%full
%header (functor TraceLexFun(structure Tokens: Trace_TOKENS));

any = [a-zA-Z0-9'_/-];
ws = [\ \t\011\012\r];

%%

\n         => (lex());
"{"        => (Tokens.LCURLY((),()));
"}"        => (Tokens.RCURLY((),()));
"["        => (Tokens.LBRACK((),()));
"]"        => (Tokens.RBRACK((),()));
"let"      => (Tokens.LET((),()));
"="        => (Tokens.EQ((),()));
"in"       => (Tokens.IN((),()));
"."        => (Tokens.DOT((),()));
"\\"       => (Tokens.LAM((),()));
"!"        => (Tokens.BANG((),()));
","        => (Tokens.COMMA((),()));
"1"        => (Tokens.ONE((),()));
"Solution" => (Tokens.SOLUTION((),()));
":"        => (Tokens.COLON((),()));
{any}+     => (mkid(yytext));
{ws}+      => (lex());

. => (raise Fail ("Unexpected character" ^ yytext));
