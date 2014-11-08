structure Parser = struct

  structure TraceLrVals = TraceLrValsFun(structure Token = LrParser.Token)
  structure TraceLex = TraceLexFun(structure Tokens = TraceLrVals.Tokens)
  structure TraceParser = Join(structure ParserData = TraceLrVals.ParserData
                               structure Lex = TraceLex
                               structure LrParser = LrParser)


  (* 
  * Takes a filename and returns a stateful function from int to string.
  * : string -> (int -> string) *)
  fun makeReader fname =
    let
      val ins = TextIO.openIn fname
    in
      fn n => TextIO.inputN (ins, n)
    end

  fun parseFile fname =
  let
    val ECC_LOOKAHEAD = 15
    val reader = makeReader fname
    val tokstream = TraceParser.makeLexer reader
    fun error_handler (error, (), ()) = print error
    val (result, _) = 
      TraceParser.parse (ECC_LOOKAHEAD, tokstream, error_handler, ())
  in
    result
  end
end
