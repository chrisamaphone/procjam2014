structure Tokens = Tokens
type pos = unit
type svalue = Tokens.svalue
type lexresult = (svalue, pos) Tokens.token
val eof = fn () => Tokens.EOF ((), ())
type ('a, 'b) token = ('a, 'b) Tokens.token

fun mkid str = 
   if (String.sub (str, 0) = #"X")
      then (case Int.fromString (String.extract (str, 1, NONE)) of
               NONE => Tokens.ID (str, (), ())
             | SOME i => Tokens.XID (i, (), ()))
   else Tokens.ID (str, (), ())

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
"("        => (Tokens.LPAREN((),()));
")"        => (Tokens.RPAREN((),()));
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
