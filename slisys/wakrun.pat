"/Clear sleep conditions for wake and exit in child/
scnser:
config+40/	
pat0:.!movei t2,0
.+1!ldb s,jbywak
.+1!cain s,36.
.+1!dpb t2,jbywak
.+1!ldb s,jbyfex
.+1!cain s,36.
.+1!dpb t2,jbyfex
.+1!jrst ttsclr+3
.+1!pat1:
ttsclr+2/jrst pat0
pat0k
"/Do not arm WAKFLG in JBTSTS if TAKTRP is called
in wake clubs/
clubs:
pat1!pat0:ldb t1,jbywak
.+1!jumpn t1,taktrp
.+1!movei t1,wakflg
.+1!iorm t1,jbtsts(j)
.+1!popj p,
.+1!pat1:
kcbwak+4/jrst pat0
"/in wake uuo/
picon:
wakuuo+10/jrst pat0
pat0k
"/in exit/
errcon:
pat1!pat0:ldb t1,jbyfex
.+1!jumpn t1,hlttrp+7
.+1!movei t1,wakflg
.+1!iorm t1,jbtsts(j)
.+1!jrst ipopj1
.+1!pat1:
hlttrp+3/jrst pat0
pat0k
"/Clear WAKFLG in logout/
clock1:
jsclr/q+wakflg
config 40/(q)+pat1-q,,pat1
./
pat1k
7tcnftbl 2/((q)+20)
 