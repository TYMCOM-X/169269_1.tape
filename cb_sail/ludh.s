Simple Integer Procedure LudHash;
Start!code
    define T1=1, T2=2, T3=3, T4=4, I=5, J=6, K=7, A='13, B='14, C='15;
    label Hash, Rnd1, Rnd2;
	MOVE	T1,USER1;
	MOVE	T2,USER2;
	SETZ	I,;
	MOVE	A,['555555555555];
	MOVE	B,['361275431652];
	MOVE	C,['612754316523];
	MOVEI	J,4;			! times to loop;
HASH:	ADD	B,T1;
	ROTC	T1,-'22;
	MOVEI	K,5;
RND1:	MOVE	T3,B(I);
	MUL	T3,[5*5*5*5*5*5*5*5*5*5*5*5*5*5*5];
	ADDM	T4,C(I);
	AOJE	I,RND2;
	MOVNI	I,1;
	TRNE	B,1;
	SKIPL	C;
	MOVEI	I,0;
	EXCH	A,C;
RND2:	SOJG	K,RND1;
	SOJG	J,HASH;
	XOR	C,B;
	XOR	C,A;
	MOVE	T1,C;
end;


 