	implicit integer (a-z)
	parameter lnorm=60,cnorm=3
	double precision infile,ofile
	integer page(8,3,60)
	type11
11	format(' input file: ',$)
	accept12,infile
12	format(a10)
	type13
13	format(' output file: ',$)
	accept12,ofile
	open(unit=1,access='seqin',file=infile)
	open(unit=2,access='seqout',device='dsk',file=ofile)
1	type4
4	format(' normal 40x60x3? <cr>=yes ',$)
	accept5,reply
5	format(a1)
	nlpp=lnorm
	ncol=cnorm
	if(reply.eq.' ')goto25
	type2
2	format(' number of lines per page: ',$)
	accept*,nlpp
	type3
3	format(' number of columns to build: ',$)
	accept*,ncol
25	do 20 npage=1,ncol
	do 20 nrow=1,nlpp
20	read(1,6,end=99)(page(i,npage,nrow),i=1,8)
6	format(8a5)

	write(2,7)(((page(i,npage,nrow),i=1,8),npage=1,3),
	1 nrow=1,60)
7	format(8a5,6x,8a5,6x,8a5)
	goto25

99	n=npage-1
	if(n.eq.2)write(2,37)(((page(i,npage,nrow),i=1,8),npage=1,n),
	1 nrow=1,60)
37	format(8a5,6x,8a5)
	if(n.eq.1)write(2,38)(((page(i,npage,nrow),i=1,8),npage=1,n),
	1 nrow=1,60)
38	format(8a5)
	close(unit=1)
	close(unit=2)
	call exit
	end
  