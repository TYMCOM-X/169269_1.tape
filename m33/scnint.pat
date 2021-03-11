"/  file	scnint.pat	WRS  29-sep-83  /
"/  purpose  /
"/    1. performance counters:  /
"/	scntow - # times SCNINT called by TOWAIT  /
"/	scndef - # times SCNINT called by USRXIT  /
"/	scnimm - # times SCNINT called by CLKINT  /
"/    2. defers SCNINT until USRXIT (based on TIMEF) if it can't be  /
"/	called at CLKINT due to user mode interlock  /
pat!scntow:0
scnimm:0
scndef:0
pat:
scnser:
pat!towa..:aos scntow
pushj p,scnint
jrst towait+15
pat:
towait+14/jrst towa..
uuocon:
pat!usrx..:aos scndef
pushj p,scnint
pushj p,uschd1
jrst usrxt7-1
pat:
usrxt7-2/jrst usrx..
clock1:
pat!cip5..:aos scnimm
pushj p,scnint
jrst cip5a+7
pat:
cip5a+6/jrst cip5..
    