File 1)	DSK:TS.DOC	created: 1405 14-JAN-83
File 2)	DSK:SPLFIX.DOC	created: 1737 12-JAN-83

1)1		TS:  A Program for Easier MTS Time Sheet Preperation
1)		----------------------------------------------------
1)		TS is designed for easier user input for the MTS time sheet.  TS
1)	reads in the time sheet file from the user's directory, sets up data
1)	structure for various records, displays these records, and prompts the
1)	user for inputs.  TS can be viewed as a limited version of a screen
1)	editor especially for time sheet files, and ONLY for HP2621 terminals.
1)		CAUTION - TS will display AT MOST 40 projects on the screen  due
1)	to the limitation of the screen memory.  The 41th project and so on WILL
1)	NOT BE DISPLAYED on the project menu.
1)		TS features:
1)		1) abilities to add projects, tasks, remarks and history
1)		   records.	
1)		2) display total hours for all the projects, and each
1)		   individual project.
1)		3) a save command to write a MMMDD.TS file so the current
1)		   editing of time sheet file can be saved.
1)		4) calculates the default date of time sheet file, i.e.
1)		   previous Friday if user runs TS on Sunday and Monday;
1)		   current Friday for all other weekdays and Saturday.
1)		5) prints out warning message of negative task hours when TS
1)		   writes out time sheet file.
1)		6) processes the single and double quotes of history records to
1)		   make them acceptable by the data base when TS writes out
1)		   time sheet file.
1)		Running TS
1)		----------
1)		TS is on directory MPL on system 36.  The user can run the
1)	program by typing "r (MPL) TS" at the monitor level.  If there is a
1)	MMMDD.TS file in user directory, TS asks user whether to edit such
1)	file.  If the user responds is "no", TS asks whether to edit the default
1)	time sheet file MMMDD.TIM.  The user can type either a carriage return
1)	or a speficic date.  If the file of the user specified date is not
1)	found, TS keeps on prompting user for time sheet file name until such
1)	file is found or user exits by typing ^C.
1)		TS then displays the project menu which is a screenful of all
1)	the projects taken from the time sheet file.  User can type "?<carriage
1)	return>" to see the help message about all the commands available.  The
1)	following is the exact help message displayed:
1)	********************************************************************
1)	* Enter one of:							   *
1)	* 	<ID> of project to see TASK menu for that project	   *
1)	*	ALL to see the complete project menu, 		           *
1)	*	    otherwise only projects with current tasks will appear *
1)	*              P,<project description> to add a project		   *
1)	*	T,<project description> to add a TEMPORARY project	   *
1)	*	    which goes away automatically upon program exit	   *
1)2	*	D,<project id> to delete a TEMPORARY project which	   *
1)	*	    was added by the previous command			   *
1)	*	REFRESH or ^L to refresh  the screen	   		   *
1)	*	SAVE to write a MMMDD.TS file				   *
1)	*	QUIT to leave the program				   *
1)	*	W to quit with expanded timesheet file - more white space  *
1)	*	? to see this message					   *
File 1)	DSK:TS.DOC	created: 1405 14-JAN-83
File 2)	DSK:SPLFIX.DOC	created: 1737 12-JAN-83

1)	*								   *
1)	*      (to add task or history, read task help message)		   *
1)	*  (type a return to continue with program)			   *
1)	********************************************************************
1)		User inputs task hours and adds history record by seeing task
1)	menu for a particular project.  The task menu can be displayed by typing
1)	the project id followed by a carriage return (see the first line on the
1)	project help message).  At task menu level, all the available commands
1)	can be displayed by typing "?<carriage return>".  The following is the
1)	exact task help message displayed:
1)	**********************************************************************
1)	* Enter one of:							     *
1)	*  <task id>,<hours> to charge hours to task(added to previous value)*
1)	*  <task id>=<hours> to set hours for task (replaces previous value) *
1)	*  <task id>,,<remain> to estimate hours remaining to completion     *
1)	*   <task id>,<hours>,<remain> to do both of above  		     *
1)	*  BEGIN <mmddyy> to set the begin date				     *
1)	*  FINISH <mmddyy> to set the finish date   			     *
1)	*  H to display or add history record				     *
1)	*  D to delete history record					     *
1)	*  REFRESH or ^L to refresh the screen		     		     *
1)	*  R,<task id> to see task remarks record			     *
1)	*  R,<task id>,<task remarks record>				     *
1)	*	to add or rewrite task remarks record			     *
1)	*  TASK <task id> to add a record for a task			     *
1)	*  QUIT or ^M to return to the project menu			     *
1)	*  ? to see this message					     *
1)	*								     *
1)	* (to add project, read project help message)			     *
1)	* (type a return to continue with program)			     *
1)	**********************************************************************
1)		TS handles illegal user input by ringing bell and erasing the
1)	line user just inputed.
1)3		Overview of the Implementation of TS
1)		------------------------------------
1)		See the description at the beginning of the source file.
1)		Overview of the Global Structures
1)		---------------------------------
1)		The information about the projects are contained in records of
1)	class PROJ.  These consist of the project id, the project begin and
1)	finish date, project history, and the initial total project hours and
1)	current project hours.  PROJ also contains two pointers LINK and TASK1.
1)	LINK links projects together and TASK1 is the pointer of each project
1)	to point to the first task record.
1)		The information about the tasks are contained in records of
1)	class TASK.  These consist of task id, task remark, task hours, task
1)	remaining hours, and a pointer LINK to link tasks together.
1)		This should be enough information to start with TS.  See the
1)	source file for details of the implementation.  
****
2)1		SPLFIX: A Program to Do Spool Maintenance
2)		-----------------------------------------
2)		SPLFIX is a SPOOL data base maintenance tool for deleting
2)	printed files and correcting common SPOOL data base problems.
File 1)	DSK:TS.DOC	created: 1405 14-JAN-83
File 2)	DSK:SPLFIX.DOC	created: 1737 12-JAN-83

2)		When to Run SPLFIX
2)		------------------
2)		SPLFIX should be run either when there is a problem running
2)	SPOOL or when disk space is low on a system.
2)		The most common SPOOL error, "CANT FIND FD", usually can be
2)	corrected by running SPLFIX and giving the command "repair".  
2)		The command "clean" will delete the files for all requests
2)	printed before the day it is run (these would be deleted in a few days
2)	with the normal updating routine but the space may be needed).
2)		How to Run SPLFIX
2)		-----------------
2)		SPLFIX is on directory UTIL.  The user can run the program by
2)	typing "r (UTIL) SPLFIX" at the monitor level.  The program will print
2)	out a header message such as:
2)		****************************************************************
2)		* SPOOL Data Base Maintenance Tool, v. 2.600                   *
2)		* System 36 (Cupertino)    10-JAN-83 09:39                     *
2)		*                                                              *
2)		* Option (? for help) *                                        *
2)		****************************************************************
2)		The user can type a "?<cr>" after the prompt "*" to see the
2)	following help message:
2)		****************************************************************
2)		* Legal commands are:                                          *
2)		*                                                              *
2)		* CLEAN (DIRECTORY) HELP        QUIT         REPAIR (DATABASE) *
2)		* Option (? for help) *                                        *
2)		****************************************************************
2)		To see a more detailed help message displayed on the screen,
2)	the user can type "HELP<cr>".  The following is the help message
2)	displayed:
2)		****************************************************************
2)		* CLEAN      Use this to DELETE unwanted files on the (SPOOL)  *
2)		*            directory.                                        *
2)		*                                                              *
2)		* HELP       Repeat this message.                              *
2)		*                                                              *
2)2		* QUIT       Graceful exit from the program.  If you exit some *
2)		*            other way, you will not get the appropriate files *
2)		*            properly updated.                                 *
2)		*                                                              *
2)		* REPAIR     Use this to rebuild SPOOL's data base.            *
2)		*                                                              *
2)		* Type any character to continue [CONFIRM]                     *
2)		************************************************************
2)		The only legal commands which the user can input following the
2)	prompt "*" are:  "?", "CLEAN", "HELP", "QUIT" and "REPAIR".  The user
2)	needs only to type minimum number of characters for the inputs.  Since
2)	all commands have unique first characters, typing the first character
2)	of a command will be sufficient.  For example, typing "C<cr>" or
2)	"CL<cr>" or "CLE<cr>" will cause the program to execute the "CLEAN"
2)	command.  But if the user types in a mispelled command such as "CLEAM",
2)	the program will output the following message:
2)		****************************************************************
File 1)	DSK:TS.DOC	created: 1405 14-JAN-83
File 2)	DSK:SPLFIX.DOC	created: 1737 12-JAN-83

2)		* CLEAM is not a command.  Legal commands are:                 *
2)		* CLEAN(DIRECTORY) HELP        QUIT          REPAIR (DATABASE) *
2)		* Option (? for help) *                                        *
2)		****************************************************************
2)		Repair Command
2)		--------------
2)		After the user types in "REPAIR<cr>", SPLFIX will try to
2)	disable SPOOL in order to do the repair job.  SPLFIX will not disable
2)	SPOOL if someone is running SPOOL, in such case SPLFIX will ask the
2)	user whether it should go to sleep for 15 seconds.  If the user response
2)	is "yes", SPLFIX goes to sleep for 15 seconds, then wakes up and tries
2)	to disable SPOOL again.  If the user response is "no", SPLFIX will print
2)	out the message "SPOOL repair was not done" and prompt the user for the
2)	next command input.  Beware that in such a case the SPOOL repair job was
2)	not done!  The user should try to run SPLFIX with the "repair" command
2)	in later time.
2)		The "repair" command also automactically does a "clean" command
2)	to delete printed files.  Therefore there is no need for the user to
2)	issue a "clean" command after the SPOOL repair job is completed.
2)		CAUTION:  SPLFIX's REPAIR command disables SPOOL to do the
2)	repair job.  It will enable SPOOL when the repair job is done.  Whenever
2)	SPLFIX finishes a repair command, the user should RUN SPOOL IMMEDIATELY
2)	AFTER EXITING FROM SPLFIX.  This is to make sure that SPOOL has been
2)	enabled by SPLFIX.  If the user types in "SPOOL", and "?SPOOL NOT FOUND"
2)	appears on the screen, please immediately contact the TYMCOM-X Support
2)	Line (408) 446-6709.
2)		What to Do In Case of Problems
2)		------------------------------
2)		When SPLFIX runs into problems during execution, it prints out
2)	the following message on the terminal and returns the user to the
2)	monitor level:
2)3		****************************************************************
2)		* Cannot continue REPAIR!                                      *
2)		* Please call the TYMCOM-X Support Line : (408) 446-6709       *
2)		* .                                                            *
2)		****************************************************************
2)		SPLFIX will print out "Cannot continue CLEAN!"  or "Cannot
2)	continue REPAIR!"  depending on which command it is executing.  In either
2)	case, call the TYMCOM-X Support Line immediately so the problem can be
2)	fixed.
2)		Sample Session
2)		--------------
2)	.r(UTIL)SPLFIX
2)	SPOOL Data Base Maintenance Tool, v. 2.600
2)	System 36 (Cupertino)    11-JAN-83 09:08
2)	Option (? for help) *clean
2)	Scanning the SPOOL data base...
2)	Will not keep files for requests completed before 10-JAN-83
2)	Found 3 files to keep.
2)	4 files were deleted.
2)	Option (? for help) *repair
2)	Checking the status of SPOOL . . . . . . No one running SPOOL
2)	Disabling SPOOL . . . SPOOL disabled.
2)	Beginning SPOOL data base repair at 11-JAN-83 09:08.
File 1)	DSK:TS.DOC	created: 1405 14-JAN-83
File 2)	DSK:SPLFIX.DOC	created: 1737 12-JAN-83

2)	Beginning pass 1:
2)	  Scanning the GDF file... 
2)	  Scanning the FDF file... 
2)	  Scanning the internal records...
2)	  Pass 1 statistics:
2)	    Scanned 100 GDF records and found 100 good ones.
2)	    Scanned 111 FDF records, and saved 111 for Pass 2.
2)	    Dates on the good records ranged from 2-DEC-82 to 10-JAN-83.
2)	    No bad records detected.
2)	Beginning pass 2...
2)	  Scanning FDF file...
2)	  Scanning GDF file...
2)	    All 100 GDF records were used.
2)	    All 111 FDF records were used.
2)	Renaming files, wait... done!!
2)	Updating SPLCNT and REMCNT, wait... done!!
2)	Cleaning the SPOOL directory, wait... 
2)	Scanning the SPOOL data base...
2)	Will not keep files for requests completed before 10-JAN-83
2)	Found 3 files to keep.
2)4	0 files were deleted.
2)	Enabling SPOOL . . . Enabled.
2)	Data base repair completed at 11-JAN-83 09:08
2)	Option (? for help) *quit
2)	.SPOOL
2)	SPOOL 6.2
2)	:quit
2)	.
**************
    aW?