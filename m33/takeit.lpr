

























				  _ _ _	_ _ _                                  T_A_K_E_I_T
				   _________                                   Operators
				_________ ______                                Reference Manual

				   version 46

			       February	24, 1981
				William	R. Soley











    ________    Abstract:

    The	primary	 function of TAKEIT  is	to provide  a means for	 orderly and
    automated takedown of a Tymcom-X system.

    TAKEIT also	provides several utility functions allowing the	 operator to
    control  timesharing and  selectively remove  users	or  groups  of users
    without necessarily	 taking	the  system down. When	the system  is being
    taken down,	TAKEIT will periodically notify	users of the time remaining.
    It also provides for a grace period	to allow users to finish  editing or
    other critical tasks. An automatic takedown	normally requires  no action
    from the operator once  started, however, any amount of  manual override
    may	be exercised at	any time.


    Tymshare, Inc.					   TAKEIT version 46


			       _____ __	________                               Table of	Contents




    Chapter								Page


	Acknowledgments	 iii

    I.	  Introduction	1
	I.1    Manual Mode  1
	I.2    Automatic Mode  1
	I.3    Exempt Jobs  2
	I.4    Immune Jobs  2
	I.5    Job-Spec	Syntax	2

    II.	  Overview of Operation	 4
	II.1   Giving your Oper	Name  4
	     II.1.1  Oper Command  4
	II.2   Entering	Commands  4
	II.3   Leaving the Program  5
	II.4   Response	to Console Interrupts  5
	II.5   Response	to being killed	 5
	II.6   Response	to Command Port	Disconnect  5
	II.7   Reattaching to a	Detached TAKEIT	 5
	II.8   Log of Actions on CTY  6

    III.  Phases of Automatic Takedown	7
	III.1  Initialization Phase  7
	III.2  Countdown Phase	7
	     III.2.1 SYS:ACCESS.MSG  7
	     III.2.2 Warning Messages  8
	III.3  Grace Phase  8

    IV.	  Self Documentation  9
	IV.1   Help Command  9
	IV.2   Instructions Command  9
	IV.3   Question	Mark as	an Argument  10
	IV.4   Remark Command  10
	IV.5   Configuration Command  10

    V.	  Query	Commands  11
	V.1    Systat Command  11
	V.2    Watch Command  12
	V.3    Control-T Command  12

    VI.	  Automatic Takedown Commands  13
	VI.1   Automatic Command  13
	     VI.1.1  Enter your	initials  13
	     VI.1.2  Enter reason for takedown	14
	     VI.1.3  Enter takedown type  14
	     VI.1.4  Enter takedown time  15
	     VI.1.5  Enter grace period	 15


				       i


    Tymshare, Inc.					   TAKEIT version 46


	     VI.1.6  Enter monitor filename  15
	VI.2   Reload Command  15
	VI.3   Exempt and UnExempt Commands  16
	VI.4   Immune and UnImmune Commands  16
	VI.5   Set Command  16
	VI.6   Grace Command  16
	VI.7   Action Command  16
	VI.8   Hold Command  17
	VI.9   Continue	Command	 17
	VI.10  Cancel Command  17
	VI.11  Warning Command	17

    VII.  Manual Control of Jobs  18
	VII.1  Talk Command  18
	VII.2  Send Command  18
	VII.3  Detach Command  19
	VII.4  Attach Command  19
	VII.5  Clear Command  20
	VII.6  Kill Command  20

    VIII. Manual Control of the	System	22
	VIII.1 Schedule	Command	 22
	VIII.2 Hang Command  22
	VIII.3 Crash Command  23

	Appendix A
	   Time	Line of	Typical	Automatic Takedown  24

	Appendix B
	   Version 46 Command Help File	 25

	Appendix C
	   Version 46 Instruction File	26

	Appendix D
	   Version 46 Job-Spec Help File  27

	Appendix E
	   Example of Automatic	Command	 28

	Appendix F
	   Example of Automatic	Takedown  30

	Index  32












				       ii


    Tymshare, Inc.					   TAKEIT version 46


				_______________                                Acknowledgments





    My thanks to  Ernie	Socci, Rick Daniels,  Ken Carta, the  PDP-10 monitor
    group and the PDP-10 operations  group for their aid in  testing TAKEIT.
    These people have also contributed suggestions regarding the behavior of
    TAKEIT under various conditions and	regarding its user interface.

    My thanks to Art Atkinson, Scott Daniels and Mike Hinckley for their aid
    in preparing this document.


								  Bill Soley








































				      iii


    Tymshare, Inc.		   Chapter I		   TAKEIT version 46


				   _______ _                                   Chapter I

				  ____________                                  Introduction




    The	primary	purpose	of TAKEIT is to	allow the operator  to automatically
    bring timesharing to an orderly  halt on any Tymcom-X. The	program	also
    provides various utility functions which may be used even if  the intent
    is not to stop timesharing.	The program is thus considered to operate in
    two	modes, Automatic Mode and Manual Mode.



	   ______ ____    I.1	   Manual Mode

    When the program is	in  manual mode, it responds only to  commands given
    by the operator. This mode requires	very little  initialization dialogue
    to	get  into  and	is  thus  useful  for  removing	 "stuck"   jobs	 and
    miscellaneous other	 features. A partial  list of manual  mode functions
    include:
	1)  Removing a single job or group of jobs
	2)  Changing the system	schedule (states)
	3)  Sending messages to	users
	4)  Setting ONCE, DSKCLN or BOOTS options for next bring-up (WC)
	    *
	5)  Having logins and logouts reported in real time
	6)  Clearing SCHPRV or JACCT for a single job (WC)
	7)  Crashing the system	for diagnostic purposes	(WC)
	8)  Hanging the	system by hand
	9)  Detaching selected jobs
	10)  Attaching to selected jobs	(WC)
	11)  Griping (reporting	bugs)
    Certain functions have been	omitted	since, although	available  in manual
    mode, make little sense to use there.



	   _________ ____    I.2	   Automatic Mode

    When  the program  is placed  in automatic	mode, TAKEIT  will  begin to
    control  the  takedown  process  automatically.  The  operator  will  be
    required to	 enter data describing	the takedown such  as when,  how and
    why.  The  automatic command  is  most  often used	to  place  TAKEIT in
    automatic  takedown	 mode. (see  Section  III.1) The  reload  command is
    actually a	special	case of	 the automatic command	and is also  used to
    enter this mode.





    * (WC) indicates Write-Core	license	is required for	this command


				       1


    Tymshare, Inc.		   Chapter I		   TAKEIT version 46


	   ______ ____    I.3	   Exempt Jobs

    Each job currently logged in is considered by TAKEIT to be either exempt
    or non-exempt. If a	job is exempt,
	1)  it will not	appear in normal systat	commands.
	2)  it	will not  be considered	 by TAKEIT  when  continuing the
	    grace period
	3)  it will not	be killed except at the	end of the  grace period
	    or by  a kill  command which  explicitly specifies	"ALL" or
	    "EXEMPT".
	4)  it	will  not  be  acted  upon  by	commands  which	 do  not
	    explicitly specify "ALL" or	"EXEMPT".

    A job is exempt if it meets	any of the following:
	1)  user PJ running PAM
	2)  a job with device ACT: assigned (CHKPNT)
	3)  the	job running TAKEIT
	4)  any	job declared to	 be exempt with	the exempt  command (see
	    Section VI.3)

    The	job which is running TAKEIT is super-exempt.  That is it will not be
    effected by	any command.  Note that	 the proper way	to "wait" for  a job
    is to make it immune (see Section VI.4).



	   ______ ____    I.4	   Immune Jobs

    Any	 job may  be declared  immune or  unimmune by  command	(see Section
    VI.4).   A	job  which  is immune  will  be	 protected  from  any action
    (manual  or	 automatic)  by	 TAKEIT.  If  an  automatic  takedown  is in
    progress,  the grace  period will  be extended  indefinitely  untill all
    immune jobs	 log out  of their own	accord or  are declared	 unimmune by
    command.  This  feature is	useful for allowing  certain jobs  to finish
    before taking the system down.  Immune jobs	will be	so marked  in systat
    output.  Only TALK,	SEND , SYSTAT and UNIMMUNE commands will work  on an
    immune job.



	   ________ ______    I.5	   Job-Spec Syntax

    Many TAKEIT	commands require an argument describing	a group	of jobs	upon
    which to act.  The syntax  for this	argument is called a  job-spec.	 The
    job-spec is	a list of  specifications separated by blanks.	Each  of the
    specifications denotes  a set  of jobs.  The  union	of  all	of  the	sets
    given on the command line is the set which is acted	upon by	the command.
    That is  to	say, any  job which is	to be acted  upon, must	 satisfy the
    criteria of	each and every specification.  Therefore, the  command "KILL
    2 3" will do  nothing since	there are no  jobs which are both job  2 and
    job	3.  Appendix D	shows a	copy of	 the job-spec help file	 obtained by
    typing a "?" in place of a job-spec.




				       2


    Tymshare, Inc.		   Chapter I		   TAKEIT version 46


    The	 specifications	 which	may  be	given  are  any	 combination  of the
    following:
	(username)	       all jobs	logged in to the given user name
	[gan,uun]	       all  users  with	 given	 Global	 Account
			   umber and Universal User Number	*
	[gan,*]		       all users  with the given  Global Account
			       Number
	job#		       the job with the	given job number
	queue		       all jobs	in the given queue or state
	device:		       all jobs	with the given device init'd
	ALL		       all jobs	(including exempt jobs)
	EXEMPT		       all exempt jobs
	NONEXEMPT	       all non-exempt jobs
	IMMUNE		       all immune jobs
	NONIMMUNE	       all non-immune jobs
	TYMSHARE	       all Tymshare in-house jobs
	BILLABLE	       all non-Tymshare	jobs
	IDLE		       all idle	jobs
	DETACHED	       all detached jobs
	WAIT		       all  jobs for  which the	 user  affirms a
			       confirmation message
    The	 lack of  either "ALL"	or "EXEMPT"  will imply	 "NONEXEMPT".  (i.e.
    unless otherwise specified,	only non-exempt	jobs are affected)

    Below are some annotated examples:
	KILL 5 (SMITH)	       kills job 5 if and only if it  belongs to
			       user SMITH and is non-exempt
	KILL TO	DETACH	       kills all jobs which are	detached,  in TO
			       state and non-exempt
	KILL <SYSTAT> ALL      kills  all  jobs	 showing  a  job-name of
			       SYSTAT

    Since all commands	implicitly exclude the	job running TAKEIT  from the
    set	of jobs	being acted  upon, commands such as those below	 become	more
    useful.
	KILL ALL	       Kill all	jobs but me
	KILL RN		       Kill all	jobs that are running (but me)















    * the pair GAN,UUN is also	know as	Accounting User	Number	(AUN) or
    as the Login Project Programmer Number (Login PPN)


				       3


    Tymshare, Inc.		  Chapter II		   TAKEIT version 46


				   _______ __                                   Chapter II

			     ________ __ _________                             Overview of Operation




    The	 TAKEIT	program	 is restricted	to those  operators who	 have either
    Write-Core (WC) or Hang-User (HU) license. It is otherwise	available by
    giving the command
	    .R TAKEIT



	    ______ ____	____ ____    II.1    Giving your	Oper Name

    If you do not have	Write-Core (WC)	license	currently set for  your	job,
    TAKEIT will	ask for	 an Oper Name. At  this	point, you should  enter the
    name given to you by your  manager for use in setting licenses.  This is
    not	to be confused with the	 name given to you by Customer	Services for
    use	in logging  in,	or with	 the name given	to  you	by your	 parents for
    people to call you by.  The	name will be looked up in  the oper-password
    file and checked for the necessary license(s).

    Next  you  will be	asked  to supply  your	Oper Password.	This  is the
    password associated	with  the Oper Name  you just entered.	The password
    will not echo.


	      ____ _______    II.1.1    Oper Command

    The	oper command is	used to	change the oper	name (and license) currently
    in effect.	This is	useful	if it becomes necessary	to use	some command
    which  requires  more license  than	 is associated	with  the  oper	name
    supplied  originally.   TAKEIT  asks the  user  to	supply	the password
    associated with the	name given  in the command.  If	this  command fails,
    the	license	previously in effect will be retained.



	    ________ ________    II.2    Entering Commands

    TAKEIT  is	purposely  designed  without  a	 command  prompt.   You	 may
    therefore assume that TAKEIT is waiting for	you to type a command unless
    it is obviously doing  something or	has asked  for you to wait.   If you
    become bewildered at some point and	need to	be assured that	 the program
    is still alive, simply type	a control-T (^T). (see Section V.3)

    Note that TAKEIT seldom allows you to type commands	or  answers "ahead",
    in order to	minimize the danger of unintentional damage due	to  a typing
    error.





				       4


    Tymshare, Inc.		  Chapter II		   TAKEIT version 46


	    _______ ___	_______    II.3    Leaving the	Program

    The	proper way to exit from	the program is to give the quit	command.  If
    the	program	is in automatic	 mode, the quit	command	will  be ineffective
    until after	the cancel command (see	Section	VI.10) has been	given.



	    ________ __	_______	__________    II.4    Response to	Console	Interrupts

    If	TAKEIT receives	 a  control-C or  escape  while	waiting	 for  you to
    complete a command or respond to a query, it will abort the	 command and
    return to TAKEIT command level.



	    ________ __	_____ ______    II.5    Response to	being killed

    TAKEIT runs	with  JACCT on which causes  the monitor to protect  it	from
    any	external force	which would cause it  to stop.	TAKEIT	also enables
    the	NTQ  interrupt (notice to  quit).  If another  job attempts  to	kill
    TAKEIT, it will receive the	 NTQ interrupt and will	respond	 by aborting
    the	takedown in a prompt and orderly manner.



	    ________ __	_______	____ __________    II.6    Response to	Command	Port Disconnect

    If the program notices that	its command port has been zapped, presumably
    due	to the connection being	lost, it will detach and continue to  run as
    if	the  operator had  given  the detach  command.	(see  Section VII.3)
    This will occur regardless of the jobs detach on disconnect	or logout on
    disconnect option in its job  privilege word.  The takedown	will  not be
    affected by	such a disconnect.  If the system is super-shut, TAKEIT	will
    shut it before detaching.



	    ___________	__ _ ________ ______    II.7    Reattaching	to a Detached TAKEIT

    A TAKEIT program that is  detached for any reason (see Section  II.6 and
    VII.3) may be reattached to	by any of
	1)  logging into the same username and giving the job  number of
	    TAKEIT to the login	program
	2)  logging into the same  username and	giving the  ATTACH <job>
	    monitor command
	3)  logging  into  any	username  and  giving  the  ATTACH <job>
	    monitor command with Jacct-Login (JL) license set
	4)  logging into  any username,	running	 TAKEIT	with  write core
	    (WC) license and giving the	command	ATTACH <TAKEIT>

    Once TAKEIT	has  been attached to, it  will	print a	welcome	 message and
    ask	the operator to	confirm	his intention of attaching to that  job.  If



				       5


    Tymshare, Inc.		  Chapter II		   TAKEIT version 46


    the	operator does so  confirm, he will be  asked to	supply an  oper	name
    and	oper password.	This will happen regardless of the fact	that the job
    might have (or  have had) Write-Core (WC)  license.	 The oper  name	must
    have  at  least  Hang-User	(HU)  license  or  Write-Core  (WC)  license
    associated with it else the	job will detach	itself again and continue to
    run.  The  name does  not need to  match the  name originally  given (if
    any).

    Once this ordeal  is over, TAKEIT will  print a brief status  report and
    enter  command  mode.  The	takedown  is not  affected  by	attaching or
    detaching in this manner.  If the system was super-shut when  TAKEIT was
    detached from, it will be automatically super-shut at this point.



	    ___	__ _______ __ ___    II.8    Log	of Actions on CTY

    Immediately	 prior to  a  HANG or  CRASH (either  manual  or automatic),
    TAKEIT will	send  a	message	to the	CTY device containing  the following
    information:
	1)  The	reason for takedown
	2)  The	username (or oper-name)	of the user running TAKEIT
	3)  The	date and time

    Occasional very severe error messages from TAKEIT may also appear on the
    CTY.  Most of these	are cryptic and	indicate extreme illness.






























				       6


    Tymshare, Inc.		  Chapter III		   TAKEIT version 46


				  _______ ___                                  Chapter III

			  ______ __ _________ ________                          Phases of Automatic Takedown




    Once  automatic  takedown  mode  has  been	entered	 with  the automatic
    command, it	will proceed based on a	countdown set by the  operator.	 The
    countdown represents the number of minutes left before the system should
    come down.	After  the countdown passes  zero (0) and  becomes negative,
    the	takedown is said to be in the grace period.



	     ______________ _____    III.1    Initialization Phase

    The	initialization	phase is  begun	by the	automatic command  or reload
    command  (see   Section  VI.1  and	 Section  VI.2).   The	 purpose  of
    this phase is to obtain  and verify	all of the variables  concerning the
    takedown:
	1)  the	initials or name of the	operator
	2)  the	reason the system is going down
	3)  the	time the system	is to go down
	4)  the	length of the grace period
	5)  the	name of	the file to reload the monitor from
	6)  the	method of taking the system



	     _________ _____    III.2    Countdown Phase

    Once the automatic command has been	given and the operator confirms, the
    takedown  enters the  countdown phase.   During this  time,	 TAKEIT	will
    periodically warn all users	of the time remaining in the  countdown	(see
    Section  III.2.2).	 *  It	will  also  update  the	 file SYS:ACCESS.MSG
    every minute  to inform new	 users of the  time left.  The	program	will
    enable watching as if the  operator	had given the watch  enable command.
    This will cause  the operator to be	 notified of all logins	 and logouts
    occurring during this phase.

    When the  countdown	reaches	 one (1)  minute, TAKEIT  will automatically
    shut the system.


	       ______________    III.2.1    SYS:ACCESS.MSG
    The	login program checks for the file SYS:ACCESS.MSG and if	it is found,
    types  its contents	 during	login.	 TAKEIT	uses  this file	 to  cause a
    message of the form
	System will leave timesharing in MMM minutes.
	System going down for fun. /me

    * Periodic warning messages	are issued automatically at 30,	10, 5,	2, 1
    and	0 minutes.


				       7


    Tymshare, Inc.		  Chapter III		   TAKEIT version 46


    While  TAKEIT is  using this  file,	it  renames any	 previously existing
    ACCESS.MSG file into  TAKEIT.MSG on	SYS:.  The  file is renamed  back at
    the	end of the TAKEIT session.

    If an automatic takedown is	 terminated in an abnormal manner such	as a
    system crash, the ACCESS.MSG may remain.  If when the system  is brought
    up again, a	message	prints at login	of the form
	System will leave timesharing in MMM minutes.
	System going down for fun. /me
    then the message  must be removed by  hand after verifying that  it	does
    not	apply.	This is	done by
	1)  setting WF license
	2)  DELETE (SYS)ACCESS.MSG
	3)  RENAME (SYS)TAKEIT.MSG,(SYS)ACCESS.MSG ;okay if this fails


	       _______ ________    III.2.2    Warning Messages
    TAKEIT periodicly sends broadcast  messages	to all terminals  during the
    countdown phase.  These messages appear to the users as
	;;OPR: - System	will leave timesharing in MMM minutes.	/me
    The	messages  may also be  sent using the  warning command	(see Section
    VI.11).



	     _____ _____    III.3    Grace Phase

    During automatic countdown,	the grace phase	or grace period	occurs after
    the	countdown has  expired (become negative).   At the beginning  of the
    grace period, TAKEIT  will automatically super-shut	the  system.  During
    the	grace  period, TAKEIT will  wake up every  second and kill  all	jobs
    which are either
	1)  non-exempt,	non-immune and idle; or
	2)  non-exempt,	non-immune and detached

    The	grace period ends when one of the following events occurs:
	1)  all	non-exempt jobs	are logged off
	2)  the	grace period time  limit is exceeded and no  immune jobs
	    remain
    At that time, all  jobs are	killed and the	system is taken	down  in the
    manner  specified  in  the	automatic  command  (see   Section  VI.1.3).
    Note that immune jobs  will	extend the grace period	 indefinitely, while
    exempt jobs	do not affect the grace	period.













				       8


    Tymshare, Inc.		  Chapter IV		   TAKEIT version 46


				   _______ __                                   Chapter IV

			       ____ _____________                               Self Documentation




    TAKEIT will	provide	a limited amount of help to the	operator while he is
    running TAKEIT.  As	a general rule,	if you don't know what to do  at any
    point, type	a question mark	 ("?").	 At command level, this	 will invoke
    the	 brief help  file.  At	a  prompt, this	 will give  a  more detailed
    description	of the information being requested and the options available
    to the operator.



	    ____ _______    IV.1    Help Command

    The	help  command may be  invoked by typing	 either	"?" or	"HELP".	 The
    command will produce a list	 of all	commands currently known  to TAKEIT.
    The	syntax of the list is as follows:
	COMmand	(noise words) [{opt1|opt2}]{req1|req2}
    The	first three letters of the command name	are capitalized	 to indicate
    that at  least three  characters are necessary  for	TAKEIT	to recognize
    this particular  command.  *  The words shown  in parenthesis  are noise
    words that make  the meaning of the	 command word clear.  These  are not
    entered  as	part  of the  command.	Anything  inside of  square brackets
    ("[]") is an optional argument  and	may be omitted.	 Anything  inside of
    curly braces ("{}")	is a  list of arguments, separated by  vertical	bars
    ("|"), of which  one must be choosen.   The	following commands  would be
    valid given	the above help line:
	COM REQ1
	COM OPT1 REQ2
	COMMAND	OPT2 REQ2
	COMMA REQ2

    See	Appendix B for a copy of the version 46	help file.



	    ____________ _______    IV.2    Instructions Command

    The	 instructions  command	will  provide  a  relatively  verbose,	task
    oriented help message concerning itself with those tasks most  likely to
    be required.  It is	not complete, but it is	specific.  Appendix  C shows
    the	file obtained from the instructions command in version 46.







    * each command may be shortened to its point of uniqueness.


				       9


    Tymshare, Inc.		  Chapter IV		   TAKEIT version 46


	    ________ ____ __ __	________    IV.3    Question Mark as an	Argument

    Giving a  question mark ("?")  in an argument  to many commands  (and in
    answer to any prompt) will provide a further explanation of	 the options
    available at that point.

    When in doubt, type	question mark.



	    ______ _______    IV.4    Remark Command

    This command is used to annotate the printout or log.  The format is
	REMARK <text>
    This message is placed in the log, and otherwise ignored.  This  is	also
    good for other comments such as
	REMARK Have to kill PJ because its stuck
	REMARK Maint. needs the	system to swap boards.
	REMARK This is to test fast DSKCLN.



	    _____________ _______    IV.5    Configuration Command

    Many of the	attributes of  the TAKEIT program are determined  at compile
    time by several switches defined in	the source code.  Normally these are
    things which  are of little	 interest to the  user of the  program.	 The
    config  command  may be  given  to obtain  a  list of  the	compile	time
    parameters.	 Below is a list of the	parameters as they exist  in version
    46:	 These values are all subject to (and likely to) change!
    TAKEIT version 46
    Production version.
    No BAIL.
    License required: WC RC OP SY JL WF.
    SCHPRV disabled.
    Minimum takedown time is 5 minutes.
    Minimum grace period is 1 minutes.
    First warning at 30	minutes.
    System is shut at 1	minutes.
    Kill time limit is 600 seconds.
    Halt time limit is 20 seconds.
    Evict time limit is	600 seconds.
    Low	priority polling interval is 5 seconds.
    Fill line length for SEND msgs is 62 chars.












				       10


    Tymshare, Inc.		   Chapter V		   TAKEIT version 46


				   _______ _                                   Chapter V

				 _____ ________                                 Query Commands




    The	following  commands may	 be used  by the  operator to  obtain status
    information	about the takedown process or about other jobs logged  in to
    the	 system.   These commands  are	available in  both  manual  mode and
    automatic mode.



	   ______ _______    V.1	   Systat Command

    The	systat	command	allows	the operator to	 see the  state	of  all	jobs
    logged in to the system.  In some respects,	it is similar to  the systat
    program,  however,	the  arguments are  different.	 The  format  of the
    command is
	SYSTAT		       defaults	to SYSTAT NONEXEMPT HEADING
	SYSTAT <job-spec>      see Section I.5 about job-specs
	SYSTAT HEADING	       provide a brief heading
	SYSTAT <job-spec> HEADING

    If HEADING is requested, various parameters	of the takedown	are given in
    a short  header.  Below is	a sample output	 from the "SYSTAT  ALL HEAD"
    command:

    System answered.
    0:44:45 remaining.
    5 minutes grace.
    System will	be hung.
    2 non-exempt jobs.
    1  PJ	     PAM     SL	 DETACH	 Exempt
    2  OPER	     CHKPNT  TI	 DETACH	 ACT:  Jacct  Ntq  Exempt
    3  WRS	     TAKEIT  RN	 TTY74	 Jacct	SchPrv	Exempt
    4  BIGBUCKS	     ADVENT  TI	 TTY77	 $
    5  OPER	     WHO     ^C	 TTY75
    6  WRS	     SLAVE   ^C	 TTY72	 SchPrv	 Exempt

    Following  each job	 line  is a  list  of flags.   These  flags indicate
    special attributes of the job:
	$ (dollar sign):       Indicates  tat  job  is	logged	in  to a
			       billable	user name.
	device:		       Job has named device open or assigned
	Jacct		       Job  has	Jacct  set  meaning it	is  in a
			       system-critical	 process.   It	 may  be
			       immune to KILL and DETACH.
	SchPrv		       Job has SchPrv  set meaning it  is locked
			       in the high  priority run queue.	  It may
			       prevent other jobs from running.
	Ntq		       Job   is	 enabled   for	notice	 o  quit



				       11


    Tymshare, Inc.		   Chapter V		   TAKEIT version 46


			       interrupt.   It	will  be  evicted rather
			       than killed.
	Immune		       The  job	 has been  declared  immune (see
			       Section I.4)
	Exempt		       The job is exempt (see Section I.3)
	(killed)	       Job has	been killed  but has  not logged
			       out yet.
	(evicted)	       Job has been  evicted but has  not logged
			       out yet.
	(halted)	       Job has been halted but is still	running.
    See	Section	VII.5 on handling problem Jacct/Schprv jobs.

    In	a default  systat, those  jobs marked  "Exempt"	would  not  be seen.
    (Section  I.3   explains  exempt  jobs,   see  also	 Section   VI.3	 and
    16 for controlling exemptness)



	   _____ _______    V.2	   Watch Command

    The	 watch command	may  be	used  to  enable and  disable  the automatic
    reporting  of logins  and  logouts on  the terminal.   When	 watching is
    enabled, TAKEIT will  print	a message on  the terminal each	time  a	user
    logs in or out of the  system.  The	message	will contain his  job number
    and	user name.  The	forms of the watch command are:
	WATCH		       indicate	 if   watching	is   enabled  or
			       disabled
	WATCH ON	       enable watching
	WATCH ENABLE	       enable watching
	WATCH OFF	       disable watching
	WATCH DISABLE	       disable watching



	   _________ _______    V.3	   Control-T Command

    The	so-called control-T command is given by	typing a control-T character
    at command level.  TAKEIT will respond with	the countdown  time followed
    by a count of non-exempt jobs currently logged in.	This is	 also useful
    to verify that TAKEIT is alive and well and	at command level.
















				       12


    Tymshare, Inc.		  Chapter VI		   TAKEIT version 46


				   _______ __                                   Chapter VI

			  _________ ________ ________                          Automatic Takedown Commands




    There are  a group of  commands used to  control the  automatic takedown
    process.  The first	 and most important  is	the automatic  command.	 The
    remainder of the commands are  used	to manually control or	override the
    automatic  takedown.   Some	of  the	 commands documented  here  are	also
    available outside of  automatic mode but  have little value	 there.	 See
    Section III.1  for a  brief	description of	the philosophy	of automatic
    mode.



	    _________ _______    VI.1    Automatic Command

    The	automatic command  is used to place  the program in  automatic mode.
    It	provides  the  basic  takedown	information  to	 the  program.	This
    information	includes:
	1)  the	initials or name of the	operator
	2)  the	reason the system is going down
	3)  the	time the system	is going down
	4)  the	length of the grace period (see	Section	III.3)
	5)  the	name of	the file to reload the monitor from
	6)  the	method of taking the system down

    There are two forms	of the automatic command.  The long form consists of
    merely
	AUTO
    After giving  the long  form, all  of the  information is  prompted	for.
    When a piece of information	is prompted for, the user may enter a "?" to
    obtain an explanation of the current options.  This	form  is recommended
    for	the beginner,  or infrequent user.  Appendix  E	shows an  example of
    the	long form of the automatic command.

    The	short  form provides all  information on one  line.  The  short	form
    consists of
	AUTOMATIC <time>,<reason>
    <time> and <reason>	are  described below.  In this form,  all parameters
    other than <time> and <reason>  are	defaulted as indicated below  in the
    appropriate	section.

    Following is an  explanation of all	of  the	parameters of  the automatic
    command.


	      _____ ____ ________    VI.1.1    Enter your initials
    This parameter is used to sign messages sent to the	users on the system.
    The	default	is your	oper name (see Section II.1).  If you have not given
    an oper name because you are running with write-core (WC)  license,	then
    the	default	is your	login user name.


				       13


    Tymshare, Inc.		  Chapter VI		   TAKEIT version 46


	      _____ ______ ___ ________    VI.1.2    Enter reason for takedown
    This parameter is a	text  string that appears in the  SYS:ACCESS.MSG and
    in	the first  broadcast  message to  users.  It  indicates	 briefly the
    reason the system  will be down.   It may also  be used to	indicate how
    long the  system will be  down for.	 The  operator has three  options in
    reply to this prompt.
	1)  enter a <cr> to omit the message
	2)  enter a number 1-7 for one of the canned messages
	3)  enter the message up to 60 characters
    The	canned messages	may  be	seen by	giving	a "?" reply.  In  Appendix E
    one	 can find  the list  of	canned	reasons	in  the	example.   A message
    entered here should	be of the form
	System going down for ...
    (i.e. they should be a complete sentence).


	      _____ ________ ____    VI.1.3    Enter takedown type
    This is the	takedown type or  method of takedown that will be  used	when
    the	grace period expires.  Except in rare instances, this will be boots-
    loaded, which is the default.  The options are:

	Boots-loaded	       All jobs	will be	killed,	the  system will
			       come  to	an  orderly stop  and  leave the
			       console with boots loaded.

	Reload		       All jobs	will be	killed,	the  system will
			       come to	an orderly stop	 and immediately
			       resume timesharing running  the specified
			       monitor.	  DSKCLN will  not be  run.  The
			       system is typically down	for  about 50-90
			       seconds.

	Crash		       This  option is	only available	to users
			       with Write-Core	(WC) license.	All jobs
			       will  be	 killed,  the  system	will  be
			       crashed	by  deposit  in	 30.	This  is
			       intended	 for  debugging	 and emergencies
			       only!

	Stand-alone	       All jobs	will be	killed,	timesharing will
			       continue	with the system	super-shut.  The
			       job running TAKEIT  will	be the	only job
			       logged in.  This	is useful for  doing ASP
			       dumps, etc.

	Manual		       TAKEIT  will  take  no  action.	 At  the
			       beginning  of  the  grace  period, TAKEIT
			       will  be	 placed	 in  manual  mode.   The
			       operator	may then use TAKEIT  commands to
			       take the	 system	down  by hand.	 No jobs
			       will be killed (unless by hand).

    This may be	changed	during	automatic mode by giving the  action command
    (see Section VI.7).


				       14


    Tymshare, Inc.		  Chapter VI		   TAKEIT version 46


	      _____ ________ ____    VI.1.4    Enter takedown time
    This parameter determines the initial value	of the countdown.  It may be
    entered as	either the local  time the  system is to  come down,  or the
    elapsed time in minutes before the system is to come down.	To enter the
    elapsed time, simply  enter	the number of  minutes.	 To enter  the local
    time, type	an atsign  ("@") followed  by the  time	of  day	in  the	form
    hh:mm.  Either a  12 hour or  24 hour clock	may  be	used for  this time.
    Some annotated examples (assume the	local time is 16:30):
	@5:00		       30 minutes (PM assumed)
	@17:00		       30 minutes (24 hour clock used)
	@4:00		       11:30 minutes (AM assumed)
	120		       2:00 minutes (elapsed time given)
    Users  without  Write-Core (WC)  license  are not  permitted  to specify
    takedown times less	than the default (which	currently is 5 minutes).

    The	takedown time may be  changed while in automatic mode by  giving the
    set	command	(see Section VI.5).


	      _____ _____ ______    VI.1.5    Enter grace period
    The	grace  time (see Section  III.3) is given  in minutes.	This  is the
    maximum  amount  of	 time  that the	 system	 will  remain  up  after the
    countdown expires.	If the	user does not have Write-Core  (WC) license,
    the	 grace	period may  not	 be set	 lower	than the  default  (which is
    currently 1	minute).

    The	length of the grace period may be changed while	in automatic mode by
    giving the grace command (see Section VI.6).


	      _____ _______ ________    VI.1.6    Enter monitor filename
    The	monitor	filename is the	 name of the file containing the  monitor to
    reload from.  If no	directory is given, (SYS) is assumed.	No extension
    should be given, .SAV is assumed.	If no name is given, the  default is
    (SYS)SYSTEM.SAV.



	    ______ _______    VI.2    Reload Command

    The	reload command is an  abbreviated form of the auto command  which is
    used to reload the monitor.	 The form is one of
	RELOAD <time>,<monitor>
	RELOAD <time>
	RELOAD
    <time>  is the  takedown  time (see	 Section VI.1.4);  <monitor>  is the
    monitor filename (see Section VI.1.6).









				       15


    Tymshare, Inc.		  Chapter VI		   TAKEIT version 46


	    ______ ___ ________	________    VI.3    Exempt and UnExempt	Commands

    These commands are used to	turn on	and off	the exemptness	of otherwise
    non-exempt	jobs.  (see  Section  I.3 for  a definition  of	 exempt) The
    command  takes a  job-spec	(see Section  I.5) as  its  argument.  These
    commands are legal in both manual mode as well as automatic	mode but are
    documented here because this is where they are most	often used.



	    ______ ___ ________	________    VI.4    Immune and UnImmune	Commands

    There commands  declare jobs  to be	 immune	or  unimmune.  All  jobs are
    normally unimmune.	 (see Section  I.4 for a  definition of	 immune) The
    command  takes a  job-spec	(see Section  I.5) as  its  argument.  These
    commands are legal in both manual mode as well as automatic	mode but are
    documented here because this is where they are most	often used.



	    ___	_______    VI.5    Set	Command

    One	may use	the set	command	to enter a new countdown.  Caution should be
    used when doing so as it may lead to great confusion on the	part  of the
    users.  If	possible, the hold  command should be  used in lieu  of	set.
    The	 new countdown	will  take effect  immediately upon  giving  the set
    command.   See Section  VI.1.4  for	the  format of	the  countdown time.
    Typing "SET	 ?" will  give a brief	explanation of	the options  at	that
    point.  Note: shortening the  countdown without giving the	users proper
    warning  is	in  very poor  taste.  The  warning command  (Section VI.11)
    may	be used	 after the set	command	to make	the  users aware of  the new
    countdown.



	    _____ _______    VI.6    Grace Command

    The	length of of the grace period (see Section III.3) may be  changed at
    any	point while in automatic mode by giving	the grace command.  The	form
    is
	GRACE <minutes>



	    ______ _______    VI.7    Action Command

    The	type of	 action	taken at  the end of  the grace	period	(see Section
    VI.1.3) may	 be changed  by	giving the  action command.   If no  type is
    given, it will be prompted for.  An	argument of "?"	will display  a	list
    of options.	 The forms are:
	ACTION Boots-loaded
	ACTION Reload
	ACTION Stand-alone
	ACTION Crash


				       16


    Tymshare, Inc.		  Chapter VI		   TAKEIT version 46


	ACTION Manual
	ACTION ?
	ACTION



	    ____ _______    VI.8    Hold Command

    Often it  becomes necessary	to  postpone the takedown  once	it  has	been
    started.  The  hold	command	 provides a  simple means  to do  so without
    canceling the takedown.  The effect	of the command is to merely stop the
    countdown.	 The countdown	may be	resumed	with  the  continue command.
    Note:  if the takedown is to be postponed for a long period, it might be
    more desirable to cancel the takedown and restart it later.	(see Section
    VI.10)  If	you  feel  that	 the  users  should  be	 notified  that	 the
    countdown  is holding,  use	the  send command  or talk  command (Section
    VII.2 Section VII.1).



	    ________ _______    VI.9    Continue Command

    The	 continue command  is used  to restart	the countdown  after  a	hold
    command has	been given.   It is often a  good practice to warn  the	user
    that the  countdown	has  changed by	issuing	 a warning  command (Section
    VI.11) after the countdown is resumed.



	     ______ _______    VI.10    Cancel Command

    The	automatic takedown  may	be canceled  by	issuing	the  cancel command.
    TAKEIT will	be  left in manual mode	 by this command.  If  any broadcast
    messages have been	sent, cancel will cause	 another message to  be	sent
    indicating that "timesharing will continue until further  notice".	This
    command basically  does not	change	the state of  the system,  it merely
    stops the automatic	takedown wherever it happens to	be.



	     _______ _______    VI.11    Warning Command

    The	warning	command	will cause  the	TAKEIT program to issue	 a broadcast
    message to all users warning them of the current countdown.	 See Section
    III.2.2 about warning  messages.  This command is  good to give  after a
    change in the schedule of the takedown as a	result of a hold/continue or
    a set command.









				       17


    Tymshare, Inc.		  Chapter VII		   TAKEIT version 46


				  _______ ___                                  Chapter VII

			     ______ _______ __ ____                             Manual Control of Jobs




    The	commands in this group are used	to control other jobs.	 The control
    that may be	exercised includes
	1)  detaching a	job
	2)  logging out	a job
	3)  sending messages to	jobs
	4)  clearing SCHPRV (Scheduler Privilege) for a	job
	5)  clearing JACCT (Job	in critical state) for a job
    All	of the commands	in this	 group may be used in either  automatic	mode
    or manual mode.

    All	of  these commands take	 as an argument	 a job-spec.   Job-specs are
    described  in Section  I.5.	  For a	 brief description  of	the job-spec
    format, the	user may give  any of these commands followed by  a question
    mark.



	     ____ _______    VII.1    Talk Command

    The	talk  command is exactly  like the monitor's  TALK command.   It has
    three forms:
	TALK ALL <message>
	TALK JOB <job >	<message>
	TALK TTY<tty >:	<message>
    The	message	received looks like
	;;OPR: - <message>



	     ____ _______    VII.2    Send Command

    The	send command  is used to send  messages	to groups of  users.  Unlike
    the	talk command, it will support multiline	messages.  The	send command
    takes a job-spec  as an argument and  then prompts for the	message	with
    ";;".  For example:
	SEND (USER)
	Enter message (end with	^D):
	;; The body of
	;; the message is placed here
	;; taking as many lines	as necessary.
	;; ^D
	_ ____        5 jobs
    The	message	should be ended	with a control-D (^D).	The message  is	sent
    all	at once	 when the ^D  is typed.	 Standard  line	mode editing  may be
    used to edit  the line being entered.   The	message	sent is	 broken	into
    tokens and tokens are filled into 64 character lines.  This	will prevent



				       18


    Tymshare, Inc.		  Chapter VII		   TAKEIT version 46


    excessive  white space  in	messages as  well as  preventing  line wrap-
    around.  The message received is of	the form
	;;message from OPR: (ME)...
	;; The body of the message is placed here taking as many
	;; lines as necessary.
    Bells  are interspersed  liberally throughout  the message	to  wake the
    sleeping user.

    Beware!  This command will stop the	clock while waiting for	 the message
    to be typed.  The user must	never leave the	terminal in the	middle	of a
    message for	any extended period  of	time (25 seconds is  the recommended
    maximum).  The command may be  aborted by typing a control-C (^C)  or an
    escape.

    Any	monitor	level talk command given by another user specifying  OPR: as
    the	destination will be received by	TAKEIT.	 *

    During the countdown, TAKEIT will automatically send  broadcast messages
    warning the	users of the time remaining.  (see Section III.2.2)



	     ______ _______    VII.3    Detach Command

    The	 detach	 command with  no  arguments will  detach  the	current	job.
    TAKEIT will	 run completely	 by itself  without a  terminal.  It  may be
    reattached to at any time as described in Section II.7.

    The	detach command also serves to detach another job or group of jobs as
    specified by the job-spec given as an argument.  The method	of detaching
    the	job(s) is such that they  will continue	to run with the	 terminal in
    user mode (if it was).  This  is useful if there is	a job  running which
    you	would like to attach to	 but you can't get at the terminal  in order
    to detach it.



	     ______ _______    VII.4    Attach Command

    The	 attach	 command  allows  the user  to	attach	to  any	 job without
    affecting the state	of  the	job (i.e. if  it is running, it	 will remain
    running).  The command takes a job-spec as an argument.  The  TAKEIT job
    that the command was issued	to is aborted and logged out.	This command
    is useful for attaching to jobs just detached by the detach	command	(see
    Section VII.3).  This command requires write core (WC) license.







    * Beware: if there are two jobs running TAKEIT simultaneously,  only one
    of them (at	random)	will receive messages sent to OPR:


				       19


    Tymshare, Inc.		  Chapter VII		   TAKEIT version 46


	     _____ _______    VII.5    Clear Command

    The	clear command  is provided to handle  jobs which have  become stuck.
    Due	to  the	delicate nature	 of the	command,  it is	restricted  to those
    with Write-core (WC)  license.  The	effect is  to turn off any  of three
    bits which are found on in the job status word (JbtSts).  The three	bits
    affected are
	1)  JACCT  (job	in  critical process  -	immune	to ^C,	hang and
	    circuit zappers)
	2)  JACCT2 (immune to ^C)
	3)  SCHPRV (Scheduler  Privilege - may	prevent	other  jobs from
	    running)
    It is beyond the scope  of this document to	provide	an  understanding of
    the	 implications of  each	of these  bits.	 A  TYMCOM-X  monitor manual
    (YAMM) should be consulted for this	information.

    The	systat command (see Section V.1) will mark jobs	with these  bits set
    by showing the  name of the	 bit at	the end	 of the	line  describing the
    job.



	     ____ _______    VII.6    Kill Command

    The	kill command is	used to	remove jobs by forcing them to log out.	 The
    prefered way  to do	 this to  a single  job	is  by giving  the following
    command
	KILL job  (username)
    This will provide a	 safety	factor in the  event that the job  number is
    incorrect or the user  has logged out of  his own will and	someone	else
    has	logged in.

    Note that if this command is used to kill the CHKPNT program, the system
    will automatically be shut.	 This is to prevent the	alarm resulting	from
    running the	system unshut without accounting.

    TAKEIT will	not kill itself, therefore, the	command
	KILL ALL
    is useful for making the system stand alone.

    Note that TAKEIT will watch	jobs that have been killed to see  that	they
    go away.  Should they not log out within a fixed period of	time, TAKEIT
    will automatically kill them again.	 A message will	be typed to indicate
    that  this is  happening.	This will  properly handle  jobs  which	have
    alternate logout programs set up which may be malicious or bug laden.

    Below is the sequence followed by TAKEIT to	kill a given frame (job):
	1)  EVICT the frame and	wait  (up to 600 seconds) for  the frame
	    to log  off.  If the  NTQ interrupt	is  not	enabled	 for the
	    frame or if	the frame  does	not log	off within  600	seconds,
	    proceed below.
	2)  HALT the frame
	3)  wait (up to	20 seconds) for	the frame to halt.  If the frame
	    has	not halted after 20 seconds, repeat the	HALT operation.


				       20


    Tymshare, Inc.		  Chapter VII		   TAKEIT version 46


	4)  remove the frame's page 0 if one existed
	5)  create a private page 0 for	the frame
	6)  deposit a small program in the frame which will log	it off
	7)  start the frame
	8)  wait (up to	600 seconds)  for the frame to log off.	  If the
	    frame  has	not logged  off	 after 600  seconds,  repeat the
	    process starting with the HALT.

    The	time limits for	the three operations indicated above is	 designed to
    allow the job to properly respond under worst case conditions.  The	user
    is told he	has 60 seconds	of time	(guaranteed) in	 which to up  and go
    away when  he receives  a notice to	 quit or  when his  alternate logout
    program is run.  These times are subject to	change!











































				       21


    Tymshare, Inc.		 Chapter VIII		   TAKEIT version 46


				  _______ ____                                  Chapter VIII

			  ______ _______ __ ___	______                          Manual Control of the	System




    The	commands  in this group	 are the most  powerful	commands  in TAKEIT.
    They are used, with	care,  to manually control the state  of timesharing
    on the system.  The	available functions are
	1)  answering, shutting	and super-shutting the system
	2)  hanging the	system (stopping timesharing)
	3)  crashing the system	(WC) *



	      ________ _______    VIII.1    Schedule Command

    The	schedule command is used exactly as the	monitor	schedule  command is
    used.  In TAKEIT, however, the command will	take both octal	and mnemonic
    arguments describing the desired state of the system.  The	command	will
    only allow the shut	and super-shut bits to be controlled.  The syntax of
    the	command	is
	SCHED ANswer
	SCHED SHut
	SCHED SUperShut
	SCHED 0
	SCHED 400000
	SCHED 600000
	SCHED

    The	last form  of the command will	merely display the current  state of
    the	system.

    Note:  TAKEIT  keeps track	of  the	state,	if  it notices	that  it has
    changed,  it will  type a  message indicating  that	some  other  job has
    changed the	state.



	      ____ _______    VIII.2    Hang Command

    The	hang command is	used to	stop timesharing in an orderly	manner.	 The
    effects are	relatively immediate.  TAKEIT will first kill all non-exempt
    jobs.  When	all non-exempt jobs are	gone, it will kill all	exempt jobs.
    When all jobs are gone (except TAKEIT), the	system will be hung, leaving
    the	console	with boots loaded.

    Using this command with any	users logged in	is in poor taste.




    * (WC) indicates that this requires	Write-Core (WC)	license


				       22


    Tymshare, Inc.		 Chapter VIII		   TAKEIT version 46


	      _____ _______    VIII.3    Crash Command

    The	crash command is used to  crash	the system by deposit in 30  for the
    purpose of debugging the monitor or	hardware.  The command is restricted
    to users with Write-Core (WC) license.  The	effect of the command  is to
    poke a non-zero word * into	physical location 30 causing the  monitor to
    crash at the next line frequency clock interrupt.  The monitor  does not
    do any cleanup when	crashing and thus the disks are	left in	an undefined
    state (bad news).  In general this command is a loser and should only be
    used when indicated	by emergency procedures	or for debugging.

    Using this command with any	users logged in	is in poor taste.








































    * The operators name converted to  sixbit is used for the  non-zero	word
    placed in location 30


				       23


    Tymshare, Inc.		   Appendix A		   TAKEIT version 46


				   ________ _                                   Appendix A

		    ____ ____ __ _______ _________ ________                    Time Line of Typical Automatic Takedown




    The	following time line  indicates the relative occurrence of  events as
    they would occur  in a typical  automatic takedown.	 Certain  events are
    omitted for	clarity	such as	 the per-minute	warnings to the	 operator as
    well as the	warnings of logins and logouts to the operator.




    ____   ______    time   action
    16 -|- Operator runs TAKEIT, supplies Name + Password
    15 -|- Operator gives AUTO 15,1 command
	+- TAKEIT sends	15 minute warning with reason
	|
	|
	|
    10 -|- TAKEIT sends	10 minute warning
	|
	|
	|
	|
     5 -|- TAKEIT sends	5 minute warning
	|
	|
     2 -|- TAKEIT sends	2 minute warning
     1 -|- TAKEIT sends	1 minute warning and shuts system
     0 -|- TAKEIT sends	final warning
	+- TAKEIT super-shuts the system
	+- TAKEIT kills	jobs as	they become idle
	|
	|
    -5 -|- TAKEIT kills	all jobs, shuts	down CHKPNT, hangs the system
    -6 -|- CTY:	is at boots-loaded

    Note: the events  shown at -5  and -6 minutes  may occur earlier  in the
    event that all non-exempt jobs log off before -5 minutes.














				       24


    Tymshare, Inc.		   Appendix B		   TAKEIT version 46


				   ________ _                                   Appendix B

			  _______ __ _______ ____ ____                          Version 46 Command Help File




    Giving the help command or issuing a "?" at	command	level will cause the
    following help file	to be displayed	on the terminal.  (see Section IV.1)



    One	of the following:
      ACtion [{Manual|Boots|StandAlone|Crash|Reload}]
      ATtach to	<job-spec>
      AUtomatic	(takedown) [{@<time>|<mins>}[ <reason>]]
      Boots [{<string>|Manual|DEfault}]	[Hang] [Crash] [{DIsk|NODisk}]
      CAncel (takedown and notify users)
      CLear (jbtsts bits for) <job-spec>
      CONFiguration (of	TAKEIT)
      CONTinue (countdown)
      CRash (system)
      DEtach {<null>|<job-spec>}
      DSkcln [{NONe|FAst|FUll|DEfault}]	[Hang] [Crash] [{DIsk|NODisk}]
      Exempt <job-spec>
      Grace (period is)	<minutes>
      HAng (system)
      HElp [<command>]
      HOld (countdown)
      IMmune <job-spec>
      INstructions (for	use of TAKEIT)
      Kill <job-spec>
      ONce [{Manual|Auto|DEfault}] [Hang] [Crash] [{DIsk|NODisk}]
      OPer <oper-name>
      Quit (from TAKEIT)
      RELoad (monitor) [{@<time>|<mins>}[ <monitor name>]]
      REMark <text>
      SChedule (is) [{Answer|SHut|SUpershut}]
      SENd <job-spec>
      SET (countdown to) {@<time>|<mins>}
      SYstat [<job-spec>][HEADING]
      Talk [{ALL|JOB #|TTY#:}] msg
      UNExempt <job-spec>
      UNImmune <job-spec>
      WATch {ON|OFF}
      WARning
      <control-T> (type	countdown)









				       25


    Tymshare, Inc.		   Appendix C		   TAKEIT version 46


				   ________ _                                   Appendix C

			  _______ __ ___________ ____                          Version 46 Instruction File




    Giving  the	 instructions  command (see  Section  IV.2)  will  cause the
    following file to be displayed on the users	terminal in version 46.



    For	complete instructions, please see the TAKEIT manual.

    To hang jobs, one at a time, as with HANGUP, use the command
	    KILL job# (user)
    where job# is the job number, (user) is the	username of the	job.
    Other criteria may be given	to kill	a job or group of jobs,	for
    details, type "KILL	?".

    To take the	system down, in	a courteous manner, use	the command
	    AUTO
    and	answer the questions as	required.  If you are confused by a
    question, type "?" and it will be explained	in detail and reask.
    This will automatically handle notification	of users and shutting
    the	system.	 Progress reports will be given.

    For	a complete (but	brief) list of all available commands, say
	    HELP



























				       26


    Tymshare, Inc.		   Appendix D		   TAKEIT version 46


				   ________ _                                   Appendix D

			 _______ __ ________ ____ ____                         Version 46 Job-Spec Help File




    Giving any command requiring a job-spec argument followed by  a question
    mark will cause  the following file	to  be displayed on the	 terminal in
    version 46.



    Job	spec options are:
      (username)   - jobs logged into given username
      [gan,uun]	   - jobs logged into this ppn
      [gan,*]	   - jobs logged into this gan
      <program>	   - jobs running this program
      job-number   - this job
      queue-name   - jobs in this queue	(state)
      device:	   - jobs owning this device
      IDLE	   - only idle jobs
      DETACHED	   - only detached jobs
      EXEMPT	   - only exempt jobs
      NONEXEMPT	   - only non-exempt jobs
      IMMUNE	   - only immune jobs
      ALL	   - all jobs (exempt too)
      WAIT	   - ask for confirmation for each job
      UNCONDITIONAL- act regardless of any pending actions
	Note: the set of jobs acted upon by the	command	is the
	    intersection of the	sets given.
	Note: NonExempt	is implied by the absence of Exempt and	All.
























				       27


    Tymshare, Inc.		   Appendix E		   TAKEIT version 46


				   ________ _                                   Appendix E

			  _______ __ _________ _______                          Example of Automatic Command




    The	following is an	example	of the initialization phase initiated by the
    long  form of  the	automatic command.   Those portions  entered  by the
    operator are underlined.



    ____    auto
			 _    Enter your initials: ?
    One	of the following:
      <initials> - a 2-3 character signature
      <cr> - to	use your OPER name (if given)
			 ___    Enter your initials: wrs
					_    Enter reason for takedown (or "?"):	?
    One	of the following:
      <cr>
      <10-60 character message>
      <reason number (see below)>
	1. System going	down as	per schedule.
	2. System going	down to	reload monitor.
	3. System going	down for hardware maint.
	4. System going	down for software maint.
	5. System going	down for network maint.
	6. System going	down for emergency maint.
	7. System going	down for software development.
					_    Enter reason for takedown (or "?"):	2
			 _    Enter takedown type: ?
    Enter action to be taken after all jobs have been killed:
      BootsLoaded: hang	system (default)
      Crash: deposit in	30 (for	debugging only)
      Manual: wait for operator	action (left in	TAKEIT)
      Reload: reload monitor from specified file (down for about 1 min)
      StandAlone: continue timesharing with only this job (super shut)
			 ______    Enter takedown type: reload
			    _    Enter monitor filename: ?
    Enter the name of the file which contains the monitor to be	loaded
    when the system comes down.	 The file must be a .SAV file.	The
    extension should not be specified; the default directory is	(SYS).
			    ______    Enter monitor filename: newmon
			 _    Enter takedown time: ?
    One	of the following:
      mmm     -	minutes	until takedown
      @hh:mm  -	local time of takedown
			 ____    Enter takedown time: @500
				   _    Enter grace	period in minutes: ?
    Enter time to allow	active jobs to remain after Scheduled down time.
				   _    Enter grace	period in minutes: 1



				       28


    Tymshare, Inc.		   Appendix E		   TAKEIT version 46


    System C37 will leave timesharing in 48 minutes /WRS
    Grace period is 1 minutes.
    System going down to reload	monitor.
    System will	reload C37-P034/F from (SYS)NEWMON.
	      _        okay? y
    48 minutes,	4 non-exempt jobs.


















































				       29


    Tymshare, Inc.		   Appendix F		   TAKEIT version 46


				   ________ _                                   Appendix F

			 _______ __ _________ ________                         Example of Automatic Takedown




    Below is an	 example of the	complete  session of an	 automatic takedown.
    Those  portions entered  by	the  operator are  underlined.	Many  of the
    commands given below are optional and not required for the	takedown but
    are	included for sake of example.




     _ ______    .r takeit
    TAKEIT version 46 at February 24, 1981 12:55
	       ___    Oper name: wrs
    Password:
    Enter commands (type "?" for help):

    ___	___    sys	all
     1	PJ	      PAM     SL  DETACH  Exempt
     2	OPER	      CHKPNT  TI  DETACH  ACT:	Jacct  Ntq  Exempt
     4	OSNF	      DIRIT   TI  TTY75
     5	WRS	      SYSTAT  ^C  DETACH
     6	SOCCI	     EDIT10  TI	 TTY74

    ____ ____    auto 15,1
    System C37 will leave timesharing in 15 minutes /WRS
    Grace period is 5 minutes.
    System going down as per schedule.
    System will	be hung.
	      _        okay? y

    ;;OPR: - System C37	will leave timesharing in 15 minutes /WRS
    ;;OPR: - System going down as per schedule.
    15 minutes,	2 non-exempt jobs.
    14 minutes,	2 non-exempt jobs.
    13 minutes,	2 non-exempt jobs.
    12 minutes,	2 non-exempt jobs.
    11 minutes,	2 non-exempt jobs.
    ;;OPR: - System C37	will leave timesharing in 10 minutes /WRS
    10 minutes,	2 non-exempt jobs.
    9 minutes, 2 non-exempt jobs.
    8 minutes, 2 non-exempt jobs.
    7 minutes, 2 non-exempt jobs.
    6 minutes, 2 non-exempt jobs.
    ;;OPR: - System C37	will leave timesharing in 5 minutes /WRS
    5 minutes, 2 non-exempt jobs.
    4 minutes, 2 non-exempt jobs.
    * LOGIN   Job 8, user BOWLESM
    ;;TTY71: - HOW LONG	WILL THE SYSTEM	BE DOWN?



				       30


    Tymshare, Inc.		   Appendix F		   TAKEIT version 46


    ___	______    sys	tty71:
    8  BOWLESM	     RPG     ^C	 TTY71

    ____ ______	____ _____ _ _______ _ ____ _____ __ ______    talk tty71:	only about 5 minutes - just going to reload
    3 minutes, 3 non-exempt jobs.

    ;;TTY71: - OKAY, THANKS, LET ME FINISH ME EDIT PLEASE

    ______ ______    immune TTY71:
    [1 jobs]
    2 minutes, 3 non-exempt jobs.
    ;;OPR: - System C37	will leave timesharing in 2 minutes /WRS
    1 minute, 3	non-exempt jobs.
    ;;OPR: - System C37	will leave timesharing in 1 minute /WRS
    System shut
    -0 minutes,	3 non-exempt jobs.
    System super shut
    ;;OPR: - System C37	will leave timesharing immediately /WRS
    * Killed job 5, user WRS <SYSTAT> ^C
    * Killed job 4, user OSNF <DIRIT> ^C

    ___	___    sys	all
     1	PJ	      PAM     SL  DETACH  exempt
     2	OPER	      CHKPNT  TI  DETACH  ACT:	jacct  ntq  exempt
     4	OSNF	      DIRIT   TI  TTY75
     6	SOCCI	     EDIT10  TI	 TTY74	immune
     8	BOWLESM	      DDT     TI  TTY71

    ____ ___ _ _____ _ _____ __	____ ____ _____    talk job 6 Ernie - hurry up	with your edit!
    [1 jobs]
    * Killed job 8, user BOWLESM <DDT> ^C
    * LOGOUT  Job 6, user SOCCI
    * Killed job 1, user PJ <PAM> ^C
    * Evicted job 2, user OPER <CHKPNT>	^C
    ***	ACCOUNTING DEVICE RELEASED ***
    CHKPNT shutdown okay.

    System is Stand-Alone
    Hanging system...^C

    please log in:















				       31


    Tymshare, Inc.					   TAKEIT version 46


				     _____                                     Index




    abnormal termination  8		  elapsed time	15
    Accounting User Number  3		  escape  5, 19
    ACT: device	 2			  exempt command  2, 16
    action command  14,	16		  exempt jobs  2, 3
    alive and well  12
    alternate logout program  21
    alternate logout programs  20	  GAN  3
    atsign  15				  Global Account Number	 3
    ATTACH <job>  5			  grace	command	 15, 16
    attach command  19			  grace	period	2, 7, 8
    AUN	 3				  grace	phase  8
    auto command  15			  grace	time  15
    automatic command  1, 7, 13-15
    automatic command example  28
    automatic events  24		  Hang-User (HU) license  6
    automatic mode  1, 7, 13, 16	  help	9
    automatic takedown	7		  help command	9, 25
    automatic takedown example	30	  help file  25
					  help line  9
					  hh:mm	 15
    being killed  5			  hold command	17
    BILLABLE  3
    Boots-loaded  14
    broadcast message  8, 14, 17, 19	  idle jobs  3
					  immune  2
					  immune command  16
    cancel command  5, 17		  immune jobs  2, 3
    CHKPNT  2, 20			  initialization phase	28
    clear command  20			  initials  13
    config command  10			  instructions command	9, 26
    confirmation message  3
    continue command  17
    control-C  5, 19			  JACCT	 5, 11
    control-D  18			  Jacct-Login (JL) license  5
    control-T  4, 12			  JbtSts  20
    countdown  15, 16			  job line  11
    countdown phase  7			  job number  3
    Crash  14				  job privilege	word  5
    crash command  23			  job status word  20
    CTY:  6				  job-name  3
    curly braces  9			  job-spec  2, 11, 16, 18, 19, 27
					  job-spec help	 27
					  job-spec syntax  2
    deposit in 30  14, 23
    detach command  5, 19
    detach on disconnect  5		  kill command	2, 20
    detached jobs  3
    device  3, 11
    device ACT:	 2			  local	time  15
    dollar sign	 11			  Logging  6

				       32


    Tymshare, Inc.					   TAKEIT version 46







    Login PPN  3			  square brackets  9
    Login Project Programmer Number  3	  Stand-alone  14
    logins  12				  state	 3
    logout on disconnect  5		  super-exempt	2
    logouts  12				  super-shut  5, 6, 8, 22
					  SYS:ACCESS.MSG  7, 14
					  systat command  2, 11, 20
    Manual  14				  system crashes  8
    manual mode	 1, 14,	16, 17		  system unshut	without
    method  14				     accounting	 20
    minutes  15
    monitor filename  15
					  takedown type	 14
					  talk command	17, 18
    noise words	 9			  time	15
    non-exempt jobs  3,	16		  time line  24
    non-immune jobs  3			  TYMSHARE  3
    notice to quit  5, 21		  type-ahead  4
    NTQ	interrupt  5

					  unexempt command  16
    oper command  4			  unimmune command  16
    Oper Name  4, 6, 13			  Universal User Number	 3
    Oper Password  4, 6			  user name  13
    oper-password file	4		  UUN  3
    OPR:  19

					  vertical bars	 9
    PJ	2
    PPN	 3
    Project Programmer Number  3	  warning command  8, 17
					  warning message  7, 8, 17
					  watch	command	 12
    question mark  9, 10		  watching  12
    queue  3				  write	core  19
    quit command  5			  write	core (WC) license  5
					  Write-Core (WC)
					     license  6, 13, 15, 20, 22, 23
    reason  14
    Reload  14
    reload command  1, 7, 15		  C
    remark command  10			     19
    removing old messages  8		  D
					     18

    schedule command  22
    SchPrv  11
    send command  17, 18
    set	command	 15, 16
    shut  22

				       33  @`0