COMMENT this package calls: x!packet( string packet );

COMMENT this package defines;

external procedure s!EOD( integer dataClass(-1) );
external procedure s!EOF( integer dataClass(-1) );
external procedure s!EOR( integer dataClass(-1) );
external procedure s!Data( string data; integer dataClass(-1) );

COMMENT (low level routines);
external procedure end!buffer;
external procedure sendOne( integer aByte, dataClass(-1) );

    