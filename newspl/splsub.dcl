
require "  SPL support subroutine library" message;
External simple boolean procedure OpenFile(
		Reference Integer Chn, Flg; String Nam);
External simple integer procedure EnterFile(Integer Chn, Exc; procedure R );
External simple boolean procedure OpnMAP( Integer Prot, Exclusive(False));
External simple boolean procedure OpnMSC(
		String Nam; Integer Prt, Flg(False),Exc(False));
External simple boolean procedure OpnGDF( Integer Prot, Exclusive(False));
External simple boolean procedure OpnFDF( Integer Prot, Exclusive(False));
External simple procedure ClsMAP;
External simple procedure ClsMSC;
External simple procedure ClsGDF;
External simple procedure ClsFDF;
External simple string procedure Token( Reference String Str );
External simple boolean procedure SubSet( String Str, Sub );
External integer procedure LukLud(String User; Reference Integer Dist,Priv);
External simple procedure DecLic;
External simple procedure IncLic;
External simple integer procedure SetOut( Integer Chan );
External simple integer procedure GetLoc(Integer Node);
External simple integer procedure AskLoc(String Prompt);
External simple string procedure PtrLoc(Integer Loc; Reference String Phone);
External simple integer procedure GetNod;
External simple integer procedure AskNod(String Prompt);
External string procedure Daytim( Integer UseTime );
External simple boolean procedure Since;
External simple boolean procedure Ask(String Prompt);
External simple boolean procedure IsFrom;
External simple boolean procedure LicOk;
External integer procedure GetGDF( Integer GDF );
External integer procedure GetFDF( Integer FDF );
External integer procedure GetMap;
External simple integer procedure FndMap( Integer Top );
External simple procedure UpdCnt( Integer Dir, Loc(-1) );
External simple procedure FDFlst( String HeadingLine; Boolean License );
External simple procedure FDFdel;
External simple procedure SupCpy( Integer License );
External simple procedure ReqPrt( Boolean Duplicate; Integer License );
External simple procedure SetOptLine;
External simple string procedure PrtOptLine;
External simple boolean procedure SixArrow;
External simple boolean procedure GetFil;
External simple integer procedure GetReq;
External simple boolean procedure GetUsr;
External simple boolean procedure GetFrm;
External simple boolean procedure GetLst( Reference String Array SubList );

require "SPLSUB" library;

    