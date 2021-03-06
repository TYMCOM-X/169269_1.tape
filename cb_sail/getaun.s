
Internal Integer Procedure
    GetAun( Reference String S; Reference Integer Mask, Pj, Pg, Char );
begin

    Mask_ Pj_ Pg_ 0;
    Aun_ Calli( !Xwd(-1,!GTAUN), calli!GETTAB );

    If ( IsOct( S ) )
     then Pj_ GetOct( S, Char )
     else Char_ Lop( S );

    Case ( Char )
     of begin
	["*"] begin
	    Pj_ '777777;
	    Mask_ -1 lsh 18;
	  end;

	[","] If ( Pj = 0 )
		then Pj_ !lh( AUN );

	["-"] If ( S neq "]" )
		 then begin
		     Char_ -1;
		     Return( 0 );
		 end
		 else Return( Calli( !Xwd(-1,!GTPPN), calli!GETTAB ) );

	[else] begin
	    Char_ -1;
	    Return( 0 );
	  end
     end;

    If ( IsOct( S ) )
     then Pg_ GetOct( S, Char )
     else Char_ Lop( S );

    Case ( Char )
     of begin
	["*"]
	begin
	    Pg_ '777777;
	    Mask_ Mask lor '777777;
	    Char_ Lop(S);
	end;

	["]"]
	Pg_ !rh( Aun );

	[else]
	begin
	    Char_ -1;
	    Return( 0 );
	end
     end;

    If ( Char neq "]" )
     then begin
	Char_ -1;
	Return( 0 );
     end
     else Return( !Xwd( Pj, Pg ) );

end;

  