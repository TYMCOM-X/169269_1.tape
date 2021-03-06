! requires (SAILIB)SAIL.DEF and BRKINI.S ;

simple string procedure FileSpec( Reference string S, Dev;
				  Reference Boolean Dot, Err);
! ----------------------------------------------------------------------;
!									;
!	FileSpec	Read a filespec from the given string and	;
!			pass a "legal" name back to the caller.  Also	;
!			return information regarding errors, device	;
!			and "dot".					;
!									;
! ----------------------------------------------------------------------;
begin "read a file name"
    String Usr, Fil, Ext, Ppn;
    Dot _ Err _ False;					! Initially false;
    Dev _ Usr _ Fil _ Ext _ Ppn _ Null;			! Initially empty;
    Fil _ Scan( S, BrkNam, Brk );			! Read name?;
    If Brk = ":" then begin
	Lop( S );					! eat ":";
	Dev _ Fil;					! set device;
	Fil _ Scan( S, BrkNam, Brk );			! Re-read name?;
    end;
    If Brk = "(" then begin
	Lop( S );					! eat "(";
	Usr _ "(" & Scan( S , BrkUsr, Brk ) & ")";	! get username;
	If Brk neq ")" then err _ True;			! everything ok?;
	Fil _ Scan( S , BrkNam, Brk );			! then filename;
    end;
    If Brk = "." then begin
	Lop( S );					! eat ".";
	Dot _ True;					! declare extension;
	Ext _ "." & Scan( S , BrkNam, Brk );		! and read it;
    end;
    If Brk = "[" then begin
	Lop( S );					! eat "[";
	Ppn _ "[" & Scan(Str, BrkPpn, Brk ) & "]";	! read nnn,nnn;
	Lop( S );					! eat brk;
	If Brk neq "]" or Length( Usr )			! complain if bad;
	 then err _ True;				! not a "]" or (USER);
    end;
    Scan( S, BrkWht, Brk );				! clear whitespace;
    If Length(Dev) = 0 then Dev _ "DSK";		! Default to DSK;
    Return ( Usr & Fil & Ext & Ppn );			! All done;
end "read a file name";

  