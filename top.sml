structure Top =
struct

fun go infile outfile = 
let  
   val clf = Parser.parseFile infile
   val twee = CLFtoTwee.compile clf
in
   Twee.printTwee outfile twee 
end

end
