0010?	%%pat!movsi p4,-400
 0020?	loop:%%fet t1,%sat(p4)
    0025?	%%end
 0030?	%%fet t2,%sat+400(p4)
0035?	%%end
 0040?	and t1,t2
  0060?	jffo t1,.+3
0080?	lsh t1,1(t2)
    0090?	aoja p1,.-2
0100?	aobjn p4,loop
   0110?	%%end
  