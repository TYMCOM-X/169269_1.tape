BROAD .    %PICTUR 
E     .    %PICTUR 
EXP   .    %PICTUR 
EXPL  .    %PICTUR 
FILE  .    %COOKIE 
GIRL  .    %PICTUR 
LINUS .    %PICTUR 
LOIS  .    %VUE    
QUOTES.    %COOKIE 
ROOMS .    %AMAZON 
MAIL  .BOX         ;SYS, MAIL BOX file
DMS   .CAB %DMS    ;DMS, Backup source in case...
DMS   .COM %DMS    ;DMS, Comment file for DMS software
MAZE  .COM %MAZE   ;MAZE, Common definitions -- Include file
UFD   .COM         ;SYS, DMS comment file for this UFD
DMS   .COR %DMS    ;DMS, Soup correction file
DMS   .DEC %DMS    ;DMS, TOPS-10 Source
SIX12 .DEC %BLISS  
UUOSYM.DEC %DEC    
BENCH .DOC %BENCH  ;Benchmark memo
DMS   .DOC %DMS    ;DMS, DOCument file
DRT5  .DOC         ;Directory documentation updates
RUNOFF.DOC         ;Runoff documentation
TRMOP .DOC         ;DEC TRMOP.'s vs. Tymcom's AUXCALs
UUOSYM.DOC         ;UUOSYM documentation
WLD7A .DOC         ;WILD documentation
BIGCAL.F4  %BIGCAL 
GGPROG.F4  %PICTUR 
MAZE  .F4  %MAZE   ;MAZE source
MAZES .F4  %MAZE   ;MAZE source
MOVEMO.F4  %BIGCAL 
POWER .F4  %MISC   
PRTYUP.FAS %SNOBOL ;FASBOL pretty-up program
WHERE .FIN %VUE    
COMAND.FOR %AMAZON ;Command scanner routine
LIST  .FOR %BENCH  ;Program to LIST stats for 1000
M     .FOR %MAZE   ;MAZE generating program
PM    .FOR %MAZE   ;Plotter Version of MAZE
REFORM.FOR %REFORM ;Data REFORMmatter
TEST  .FOR %AMAZON ;TEST program
LIST  .HGH %BENCH  
DMS   .HLP %DMS    ;DMS, HELP file
DMSCOM.HLP %DMS    ;DMS, Commands HELP file
DMSI  .HLP %DMS    ;DMS, Individual mode HELP file
DMSIE .HLP %DMS    ;DMS, Iextension HELP file
DMSS  .HLP %DMS    ;DMS, Sort Switch HELP file
DMSSW .HLP %DMS    ;DMS, Switches HELP file
LOGON .HLP %LOGON  ;LOGON, HELP file.
RPG   .INI         ;RPG, Initialization file
SWITCH.INI         ;SYS, User Profile file
LIST  .INP %BENCH  
CRSHID.LOG %CRASH  
DMS   .LOW %DMS    ;DMS, Tymshare version
LIST  .LOW %BENCH  
H8YRZI.MA0 %MAGNUM 
1000  .MAC %1000   ;PERF, Performance for I/O
CARL  .MAC %UNV    ;Personal MACRO definitions
CKSUM .MAC %CKSUM  ;Tymshare Checksum Program
COOKIE.MAC %COOKIE ;Cookie Monster Program
D     .MAC %REVDIR ;Original Cogliano "D" directory program
DATE  .MAC %SUB    ;Get day of Week
DIRECT.MAC %DEC    ;DEC's DIRECTory program
DMS   .MAC %DMS    ;DMS, Tymshare version source file
DSKCHR.MAC %DISK   ;Program to print DSKCHR data
ECM12 .MAC %UNV    ;Version 12 of MULREA.MAC
FD    .MAC %REVDIR ;Directory program
HANG  .MAC %PENDU  ;Hangman game
HDTE75.MAC %DMS    ;DMS, Date conversion routines
KOSTER.MAC %UNV    ;Koster Universal
LEXE  .MAC %EXE    ;LOAD an EXE file on TYMCOM-X
LINKUP.MAC %LINKUP ;LINKUP, Multi-system i/o package
LOGON .MAC %LOGON  ;INIT Program
LOOKTT.MAC %MISC   ;Look at TTy for input
MJK   .MAC %UNV    ;Koster Universal
ND    .MAC %REVDIR ;Directory Program
RUNEXE.MAC %EXE    ;Load and RUN and EXE file on TYMCOM-X
TTYO  .MAC %DMS    ;DMS, Teletype output routine
TYMCAB.MAC %SUB    ;Subroutine package [CAB]
TYMSCN.MAC %SUB    ;Scanner package [CAB]
WLD7A .MAC %WILD   ;Source for version %7A
LIST  .MCH %BENCH  
H8YRZI.MD0 %MAGNUM 
U31EXB.ME0 %MAGNUM 
UB0W38.ME0 %MAGNUM 
Z3CIV9.ME0 %MAGNUM 
UB0W38.MP0 %MAGNUM 
O76M8D.MR0 %MAGNUM 
YRZUS6.MR0 %MAGNUM 
H8YRZI.MS0 %MAGNUM 
H8YRZI.MX0 %MAGNUM 
AMAZON.OFF %AMAZON ;Offsets for Amazon Structure
LOGON .OLD %LOGON  ;LOGON, Tops-10 version, ancient.
MAZE  .ONE %MAZE   ;MAZE, Normal version #1
PMAZE .ONE %MAZE   ;MAZE, Plotter version #1
BARON .PIC %PICTUR 
BROAD .PIC %PICTUR 
EXP   .PIC %PICTUR 
LINUS .PIC %PICTUR 
PEACE .PIC %PICTUR 
PUPPY .PIC %PICTUR 
SNOOPY.PIC %PICTUR 
BLOCK .REL %SUB    ;Block letter subroutine
DDT   .REL %DDT    
HDTE75.REL %DMS    ;DMS, Date conversion routines
HELPER.REL %LIB    ;System help file
LEXCMP.REL %SUB    
SCAN  .REL %SCAN   
SCN7B .REL %SCAN   ;Version %7B
TTYO  .REL %DMS    ;DMS, Teletype output routine
TYMCAB.REL %SUB    
TYMSCN.REL %SUB    
WLD7A .REL %WILD   
AUXLIB.REQ         ;WRS's Sail require file
DMS   .RND %DMS    ;DMS, Document source file
DMS   .RNH %DMS    ;DMS, HELP source file
DMSCOM.RNH %DMS    ;DMS, Command HELP source
DMSI  .RNH %DMS    ;DMS, Individual mode HELP source
DMSIE .RNH %DMS    ;DMS, Iextension HELP source
DMSS  .RNH %DMS    ;DMS, Sort Switch HELP source
DMSSW .RNH %DMS    ;DMS, Switches HELP source
LOGON .RNH %LOGON  ;LOGON, HELP FILE source
RPG   .RV1         ;RPG, Initialization file, Version 1
AMAZON.SAI %AMAZON ;Test Sail program
DUMP2 .SAI         ;MISC, Print a file in many modes
1000  .SAV %1000   ;Performance, Running program
CRA001.SAV %CRASH  
CRA002.SAV %CRASH  
CRA003.SAV %CRASH  
CRA004.SAV %CRASH  
CRA005.SAV %CRASH  
CRA006.SAV %CRASH  
FILDDT.SAV         ;FILDDT, TOPS-10 version of FILE DDT
PFD   .SAV         ;PFD, Phanthom File Directory Manipulator
DMS   .SHR %DMS    ;DMS, Tymshare version
LOGON .SHR %LOGON  ;LOGON, Universal Init program
MTADIR.SHR         ;DIRECT, DEC's version with SCAN/WILD
ADDRES.SNO %PHONE  ;Snobol program to generate an address file
DUMP  .SNO %DUMP   
LNKTYP.SNO %LINK   
P     .SNO %PHONE  
PH    .SNO %PHONE  
PHONE .SNO %PHONE  
PRTYUP.SNO %SNOBOL ;Snobol 4 pretty-up for FASBOL
RECORD.SNO %MISC   
SNOSYM.SNO %SNOBOL ;Snobol 4 list and cross-reference-er
MAZE  .TWO %MAZE   
AMAZON.TXT %AMAZON ;Dungeon definitions
COOKIE.TXT %COOKIE ;Fortune cookies!
FOOF3 .TXT %BENCH  ;Foonly F3 stats
KI10  .TXT %BENCH  ;KI-10 stats
KL10  .TXT %BENCH  ;KL-10 stats
KS10  .TXT %BENCH  ;DEC 2020 stats
MAZE  .TXT %MAZE   ;DMS file describing various MAZE programs
MOVIES.TXT %TEXT   
SOUNDE.TXT %TEXT   
SUMARY.TXT %BENCH  ;Summary of Performance stats
TRIP  .TXT %LETTER 
CARL  .UNV %UNV    
ECM12 .UNV %UNV    
JOBDAT.UNV %UNV    ;MACRO, Job data Universal file
KOSTER.UNV %UNV    
MACTEN.UNV %UNV    ;MACRO, System Macros file
MJK   .UNV %UNV    
UUOSYM.UNV %UNV    ;MACRO, UUO symbols Universal file
DMS   .UTH %DMS    ;DMS, Edit history at Univ of Tex @ Austin
DMSSW .UTH %DMS    ;DMS, UTH Switches, long.
    