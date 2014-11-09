structure Parser = struct

   structure TraceLrVals = TraceLrValsFun(structure Token = LrParser.Token)
   structure TraceLex = TraceLexFun(structure Tokens = TraceLrVals.Tokens)
   structure TraceParser = Join(structure ParserData = TraceLrVals.ParserData
                                structure Lex = TraceLex
                                structure LrParser = LrParser)

   fun parse reader =
   let
      val ECC_LOOKAHEAD = 15
      val tokstream = TraceParser.makeLexer reader
      fun error_handler (error, (), ()) = print error
      val (result, _) = 
         TraceParser.parse (ECC_LOOKAHEAD, tokstream, error_handler, ())
   in
      result 
   end

   fun parseFile fname =
   let
      val ins = TextIO.openIn fname
   in
      parse (fn n => TextIO.inputN (ins, n)) before TextIO.closeIn ins
   end

   fun parseString str = 
   let
      val pos = ref 0
      val n = size str

      fun reader i = 
      let
         val m = !pos + i
      in
         if m > n 
         then (String.extract (str, !pos, NONE) 
               before (pos := n))
         else (String.extract (str, !pos, SOME i)
               before (pos := m))
      end
   in
      parse reader
   end

end
