begin
  require "(sailib)sail.def" source!file;

  own integer a,b,c;
  own string p;

  while true
   do begin
      a_ cvfil( inchwl, b, c );
      p_ if c = 0
       then null
       else if 0 neq (!lh(c))
	   then "["&cvos(!lh(c))&","&cvos(!rh(c))&"]"
	   else "("&cv6str(memory[c])&cv6str(memory[c+1])&")";
      print( p, cv6str(a), ".", cv6str(b), '15&'12 );
   end;

end;
  