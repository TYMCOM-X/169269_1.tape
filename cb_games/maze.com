	LOGICAL DONE
	INTEGER K
	INTEGER WID,HGT,HEIGHT
	INTEGER X,Y
	INTEGER USED,LR,UD
	INTEGER LEFT,RIGHT,UP,DOWN
	INTEGER NXX,NYY
	INTEGER TINDEX, TRACKS
	COMMON	/WID/WID/HGT/HGT/HEIGHT/HEIGHT
	COMMON /USED/USED(43,100)
	COMMON /LR/LR(100,44)/UD/UD(101,43)
	COMMON /LEFT/LEFT/RIGHT/RIGHT/UP/UP/DOWN/DOWN
	COMMON /NXX/NXX(4)/NYY/NYY(4)
	COMMON /K/K
	COMMON /MAX/MAX
	COMMON /DONE/DONE
	COMMON	 /TINDEX/TINDEX/TRACKS/TRACKS(100,2)
  