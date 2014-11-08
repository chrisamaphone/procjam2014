structure Parser = struct

  structure TraceLrVals = TraceLrValsFun(structure Token = LrParser.Token)
  structure TraceLex = TraceLexFun(structure Tokens = TraceLrVals.Tokens)
  structure TraceParser = Join(structure ParserData = TraceLrVals.ParserData
                               structure Lex = TraceLex
                               structure LrParser = LrParser)

  fun parseFile fname =
  let
    val ECC_LOOKAHEAD = 15
    val ins = TextIO.openIn fname
    val tokstream = TraceParser.makeLexer (fn n => TextIO.inputN (ins, n))
    fun error_handler (error, (), ()) = print error
    val (result, _) = 
      TraceParser.parse (ECC_LOOKAHEAD, tokstream, error_handler, ())
  in
    result before TextIO.closeIn ins
  end
end
