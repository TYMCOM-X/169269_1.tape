0010?	margin 132
  0020?	randomize
   0030?	dim b7(4)
   0040?	dim t9(3)
   0050?	dim a(50,50),b(50,50)
 0060?	dim d(4,3)
  0065?	dim v(4),w(4)
    0070?	mat b=con
   0080?	data 1,-1,0,2,0,1,3,1,0,4,0,-1
  0090?	dim x(50),y(50)
  0100?	def fnn(q)=int(q/1000)
0110?	def fne(q)=int((q-1000*int(q/1000))/100)
  0120?	def fns(q)=int((q-100*int(q/100))/10)
0130?	def fnw(q)=int((q-10*int(q/10)))
0140?	mat read d
  0150?	data 1000,10,100,1,10,1000,1,100
0152?	for i=1 to 4
0155?	let b7(i)=i
 0157?	next i
 0160?	print "What size maze do you want (Square)? ";
 0170?	input e5
    0180?	let e5=e5 + 1
    0190?	print
       0200?	print
  0210?	let s2=2+int((e5-2)/2)
0220?	let t2=2+int(e5/2)
    0230?	for i=3 to e5-1
  0240?	for j=2 to e5
    0250?	if i+j=int((i+j)/2)*2 goto 2900
 0260?	let a(i,j)=10
    0270?	let a(i+1,j)=1000
0280?	goto 3100
   0290?	let a(i,j)=1000
  0300?	let a(i-1,j)=10
  0310?	next j
 0320?	next i
 0330?	let a(s2,s2)=a(s2,s2)+100
  0340?	let a(s2,t2)=a(s2,t2)+1
    0350?	for i=1 to 4
0360?	read v(i),w(i)
   0370?	next i
 0380?	for i=2 to e5
    0390?	let b(e5+1,i)=1
  0400?	let b(i,e5+1)=1
  0410?	let x(i)=i
  0420?	let y(i)=i
  0430?	next i
 0440?	rem				start loop!
    0450?	for i=2 to (1+int(sqr(e5+1)))
   0460?	let x1=x(i)
 0470?	let y1=y(i)
 0480?	let x2=2+int((e5-1)*rnd(0))
0490?	let y2=2+int((e5-1)*rnd(0))
0500?	let x(i)=x(x2)
   0510?	let x(x2)=x1
0520?	let y(i)=y(y2)
   0530?	let y(y2)=y1
0540?	next i
 0550?	for i=2 to e5
    0560?	for j=2 to e5
    0570?	let s=x(i)
  0580?	let t=y(j)
  0590?	if (s+t)=int((s+t)/2)*2 goto 6300
    0600?	if a(s,t) > 10 goto 10900
  0610?	if a(s+1,t) > 1000 goto 10900
   0620?	goto 6500
   0630?	if a(s,t) > 1000 goto 10900
0640?	if a(s-1,t) > 10 goto 10900
0650?	rem						???
0660?	for i1=1 to 2
    0670?	let t7=1+int(4*rnd(0))
0680?	for j1=1 to 3
    0690?	let t9(j1)=d(i1,j1)
   0700?	let d(i1,j1)=d(t7,j1)
 0710?	let d(t7,j1)=t9(j1)
   0720?	next j1
0730?	next i1
0735?	gosub 15000
 0740?	for z7=1 to 4
    0745?	let l1=b7(z7)
    0750?	let x5=s+d(l1,2)
 0760?	let y5=t+d(l1,3)
 0770?	if a(s,t)=0 goto 9200
 0780?	if a(x5,y5)=0 goto 9200
    0790?	if (x5+y5)=int((x5+y5)/2)*2 goto 8400
0800?	if w(d(l1,1))=10 goto 9200
 0810?	if a(x5,y5) > 10 goto 8800
 0820?	if a(x5+1,y5) > 1000 goto 8800
  0830?	goto 9200
   0840?	if w(d(l1,1))=1000 goto 9200
    0850?	if a(x5,y5) > 1000 goto 8800
    0860?	if a(x5-1,y5) > 10 goto 8800
    0870?	goto 9200
   0880?	let a(s,t)=a(s,t)+v(d(l1,1))
    0890?	let a(x5,y5)=a(x5,y5)+w(d(l1,1))
0900?	if rnd(0) < .35 goto 4400
  0910?	goto 9400
   0920?	next z7
0930?	goto 10900
  0940?	for z7=1 to 4
    0950?	let x6=x5+d(l1,2)
0955?	let l1=b7(z7)
    0960?	let y6=y5+d(l1,3)
0970?	if (x6+y6)=int((x6+y6)/2)*2 goto 10100
    0980?	if a(x6,y6) > 10 goto 10700
0990?	if a(x6+1,y6) > 1000 goto 10700
 1000?	goto 10300
  1010?	if a(x6,y6) > 1000 goto 10700
   1020?	if a(x6-1,y6) > 10 goto 10700
   1030?	let s=x6
    1040?	let t=y6
    1050?	if a(s,t)=0 goto 10700
1060?	goto 5900
   1070?	next z7
1080?	goto 4400
   1090?	next j
 1100?	next i
 1110?	let r1=2+int((e5-1)*rnd(0))
1120?	let r2=2+int((e5-1)*rnd(0))
1130?	let a(1,r1)=10
   1140?	let a(2,r1)=1000+a(2,r1)
   1150?	let a(1,r1-1)=10
 1160?	let a(2,r1-1)=1000+a(2,r1-1)
    1170?	let a(e5,r2)=a(e5,r2)+10
   1180?	let a(e5,r2-1)=a(e5,r2-1)+10
    1190?	let a(e5+1,r2)=1000
   1200?	let a(e5+1,r2-1)=1000
 1210?	let p1=e5
   1220?	let q1=r2
   1230?	for i=1 to e5
    1240?	for j=2 to e5
    1250?	if (i+j)=int((i+j)/2)*2 goto 13300
   1260?	if i=1 goto 12800
1270?	if fnw(a(i,j))=0 goto 13000
1280?	print " ";
  1290?	goto 13100
  1300?	print "/";
  1310?	print " ";
  1320?	goto 14200
  1330?	if i=1 goto 13500
1340?	if fnw(a(i,j))=0 goto 13700
1350?	print " ";
  1360?	goto 13800
  1370?	print "\";
  1380?	if fns(a(i,j))=0 goto 14100
1390?	print " ";
  1400?	goto 14200
  1410?	print "_";
  1420?	next j
 1430?	if (i+j)=int((i+j)/2)*2 goto 14600
   1440?	print "\"
   1450?	goto 14700
  1460?	print "/"
   1470?	next i
 1480?	stop
   1500?	rem						! subroutine !
    1510?	for r8=1 to 4
    1520?	let b6=1+int(4*rnd(0))
1530?	let r9=b7(r8)
    1540?	let b7(r8)=b7(b6)
1550?	let b7(b6)=r9
    1560?	next r8
1570?	return
 2000?	end
    