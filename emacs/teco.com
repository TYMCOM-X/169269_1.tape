DIST  .LET ;Distribution letter
TECO  .DIR ;This file
----------

Install on SYS:

TECO  .ERR ;TECO error file
TECO  .EXE ;Executable TECO core image
----------

Install on DOC:

TECNEW.DOC ;Describes differences between TECO 124 and TECO 124A
TECO  .DOC ;TECO command summary and hints
TEC124.DOC ;Documentation for TECO 124A
----------

Install on TED:

ACOM  .TEC ;Aligns comments in MACRO programs
CLERK .DOC ;Documentation for CLERK.TEC
CLERK .TCO ;Source for CLERK.TEC
CLERK .TEC ;Macro to create repeated blocks of text
COM   .TEC ;Adds comments to MACRO programs
COMENT.TEC ;Adds comments in MACRO programs
CONTEN.TCO ;Source for CONTEN.TEC
CONTEN.TEC ;Macro to generate table of contents for MACRO source file
CT    .TEC ;Clears screen on Minibee and ADM and does an HT
DATE  .TCO ;Source for DATE.TEC
DATE  .TEC ;Returns date in Q-reg. 0
DECIDE.TCO ;Source for DECIDE.TEC
DECIDE.TEC ;Performs text substitutions allowing user to decide each
           ;case
DOW   .TEC ;Returns day of the week
FORCOM.TEC ;Adds comments to FORTRAN programs.
GETNAM.TCO ;Source for GETNAM.TEC
GETNAM.TEC ;Returns user name
GIVMFC.TEC ;Displays amount of monitor free core in use
HA    .TCO ;Source for HA.TEC
HA    .TEC ;Brings entire file into core, preserving form feeds
HELP  .TCO ;Source for HELP.TEC
HELP  .TEC ;On-Line Help System for TECO.  Run using "EIHELP$$".
HINTS .TEC ;Little-known features and hints about TECO
HT    .TCO ;Source for HT.TEC
HT    .TEC ;Types text between form feeds. (Useful with HA.TEC)
LINE  .TCO ;Source for LINE.TEC
LINE  .TEC ;Returns current line number
MAKTEC.SAV ;Saved version of MAKTEC.TEC
MAKTEC.TCO ;Source for MAKTEC.TEC, MAKTEC.SAV
MAKTEC.TEC ;TECO macro "compiler"; Produces .TEC files from .TCO files.
PGMFMT.TEC ;Formats MACRO programs. (Example in TECO Programmer's Manual.)
PPN   .TCO ;Source for PPN.TEC
PPN   .TEC ;Returns job's PPN in Q-reg. 0
RUNCON.TCO ;Source for RUNCON.TEC
RUNCON.TEC ;Macro to generate table of contents for document produced
           ;by RUNOFF
SIG1  .TEC ;Guess what this does before trying it!
SIXBIT.TCO ;Source for SIXBIT.TEC
SIXBIT.TEC ;Converts SIXBIT argument to ASCII
SYSTEC.TEC ;Types status of system, similar to SYSTAT.
TECHLP.TXT ;Help text used by HELP.TEC. (Specially formatted version
           ;of TECO.DOC.)
TED   .DIR ;Documented directory of TED: area
TIME  .TCO ;Source for TIME.TEC
TIME  .TEC ;Returns time in Q-reg. 0
UNDER .TCO ;Source for UNDER.TEC
UNDER .TEC ;Makes underscored text readable on a CRT
----------

Source files:

TECO  .MAC ;Source to TECO 124A
TECO  .REL ;TECO relocatable binary. (Assembled with MACRO 53(1107).)
TECERR.MAC ;Source for TECO.ERR
----------

Documentation source files and formatters:

TECO  .RND ;RUNOFF source for TECO.DOC, TECHLP.TXT, HINTS.TEC, TECO.CRD
TECNEW.RND ;RUNOFF source for TECNEW.DOC
TEC124.RND ;RUNOFF source for TEC124.DOC
TECDOC.MIC ;MIC command file to produce TECO documentation
TECDOC.TEC ;Macro to prepare various documentation files.  Used by
           ;TECDOC.MIC.
TECO  .CRD ;Specially formatted version of TECO.DOC suitable for pocket
           ;reference card
TECO  .REF ;3-column wide version of TECO.CRD
3X60  .FOR ;Program to make 3-column wide version of TECO.CRD
3X60  .EXE ;Executable core image of 3X60.FOR
RUNFIL.TEC ;Processes TECO.CRD to make sure there are 60 lines per
           ;page
RUNOFF.EXE ;RUNOFF core image
RUNOFF.MAC ;RUNOFF with bug fixes by U. of Pittsburgh and U. of Texas.
           ;Needed to properly process TECO.RND
RUNOFF.DIF ;Differences between DEC and UT RUNOFF
----------

Tutorials:

TOPS06.SAV ;TECO Tutorial 1.
TOPS07.SAV ;TECO Tutorial 2.
TOPS08.SAV ;TECO Tutorial 3.
TOPS09.SAV ;TECO Tutorial 4.
ALG432.SHR ;Algol OTS for TOPS06-TOPS09
  