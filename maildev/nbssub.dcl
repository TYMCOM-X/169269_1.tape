comment

! external definitions for NBSSUB ;

external string procedure NBSDate;

external string procedure CVNDate( string NBSDate );
external string procedure CVSDate( string NBSDate );

external string procedure CVList( string S );

external name!pointer procedure FillIn( NBS!pointer N );

external string procedure Summary( NBS!pointer N );

external procedure NBSPrint( NBS!pointer N; integer Chan(-1));

  