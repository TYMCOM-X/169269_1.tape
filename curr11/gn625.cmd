daytime Command file to create files for base node 625!
delete ##625.mac,##625.cmd -gn625.cmd
run (11tools)genbas
625
50
YES
YES
59
X
YES
YES
1125
4800
2
N
0
2
0
Y
run (11tools)macn11
mc625.cmd@
run (11tools)link11
lk625.cmd@/e
run (11tools)nibtrn
SV625.IMG
SV625.NIB
dir  ##625.*
  