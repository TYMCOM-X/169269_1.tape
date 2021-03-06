
let p!queue=integer, queue!block=integer;
define offset!key=2, offset!val=1;

external p!queue procedure MER!PQ( p!queue x, y );
external p!queue procedure MEZ!PQ( p!queue x; queue!block y );
external queue!block procedure POP!PQ( reference p!queue x );

integer alloc!size;
queue!block freeQueues;

simple procedure FREE!QB( queue!block x );
begin
memory[x]_ freeQueues;
freeQueues_ x;
end;

queue!block procedure allocate!PQ;
begin	integer array room[0: alloc!size_ alloc!size max 20, 0:2];
	integer mn, loc;
mn_ location( room[0,0] );
for loc_ location(room[alloc!size,0]) step -3 until mn 
   do free!qb( loc );
memory[location(room)]_ 0;
return( freeQueues );
end;

queue!block simple procedure NEW!QB( integer val, key );
begin!code
	SKIPN	1,freeQueues;
	 PUSHJ	P,allocate!PQ;
	MOVE	2,(1);
	MOVEM	2,freeQueues;
	POP	P,2;
	POP	P,offset!key(1);
	POP	P,offset!val(1);
	JRST	(2);
end;

simple procedure setupQueues; begin alloc!size_ 150; freeQueues_ 0; end;
require setupQueues initialization[0];


! p!queue_ MER!PQ( p!queue x, y );
! p!queue_ MEZ!PQ( p!queue x, queue!block y );
! queue!block_ POP!PQ( @p!queue x );
! queue!block_ NEW!QB( integer key, val );
! FREE!QB( queue!block x );

  