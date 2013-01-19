	SUBROUTINE IN_OUTT  ( output, name, lun, nlun, devs, iret )
C************************************************************************
C* IN_OUTT								*
C*									*
C* This subroutine processes the OUTPUT variable.  The requested	*
C* output types are determined and corresponding logical unit numbers	*
C* are returned.  Output may be directed to the terminal and/or a file.	*
C* OUTPUT will be searched for 'T' and 'F' to determine the output	*
C* devices.  If the output devices are followed by a slash and a	*
C* string, the string will be used as the name of the output file.	* 
C* If file output is requested and no file name is specified, the	*
C* file will be called NAME.FIL, where NAME is an input variable to	*
C* this subroutine and should be the name of the executing program.	*
C* If no valid devices are specified, output will be sent to the	*
C* terminal.  If the output request contains an 'N' before the slash,	*
C* no output will be written.						*
C*									*
C* IN_OUTT  ( OUTPUT, NAME, LUN, NLUN, DEVS, IRET )			*
C*									*
C* Input parameters:							*
C*	OUTPUT		CHAR*		Output variable			*
C*	NAME		CHAR*		Program name			*
C*									*
C* Output parameters:							*
C*	LUN  (NLUN)	INTEGER		Logical unit numbers		*
C*	NLUN		INTEGER		Number of output devices	*
C*	DEVS (NLUN)	CHAR*1		Device name  ("T","F")		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = error opening files	*
C**									*
C* Log:									*
C* M. Goodman/RDS	 9/84	Original source IP_OUTT			*
C* M. desJardins/GSFC	 6/88	Cleaned up				*
C* M. desJardins/GSFC	 4/90	Added file name				*
C* S. Schotz/GSC	 8/90	Write warning message for no output	*
C* M. desJardins/GSFC	10/90	CALL FL_SWOP so file name will be l.c.	*
C* M. desJardins/GSFC	11/90	Save file name in outfil		*
C* K. Brill/NMC		01/92	Removed the "P" option			*	
C* B. McDonald/HPC	06/01	Changed filnam, out, carr, and outfil   *
C*				to char*160				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
C*
	INTEGER         lun (*)
	CHARACTER*(*)   output, name, devs (*)
C*
	CHARACTER       filnam*160, out*160, carr (2)*160
C*
	CHARACTER	outfil*160
	SAVE		outfil
C-----------------------------------------------------------------------
	nlun = 0
	iret = 0
	CALL ST_LCUC  ( output, out, ier )
C
C*	Break output into two strings separated by /.  The string
C*	before the / contains devices; after the / is the file name.
C
	ndex = INDEX ( out, '/' )
	IF  ( ndex .gt. 1 )  THEN
	    carr (1) = out ( 1:ndex-1 )
	    carr (2) = out ( ndex+1: )
	  ELSE IF  ( ndex .eq. 1 )  THEN
	    carr (1) = ' '
	    carr (2) = out ( ndex+1: )
	  ELSE
	    carr (1) = out
	    carr (2) = ' '
	END IF
C
C*	Check first for no output.
C
	ndex = INDEX  ( carr (1), 'N' )
	IF  ( ndex .ne. 0 )  THEN
	    CALL ER_WMSG ( 'IN', 2, ' ', ier )
	    IF  ( lunp .gt. 0 )  THEN
		CALL FL_CLOS  ( lunp, ier )
		lunp = 0
	    END IF
	    IF  ( lunf .gt. 0 )  THEN
		CALL FL_CLOS  ( lunf, ier )
		lunf = 0
	    END IF
	    RETURN
	END IF
C
C*	Check for terminal output.
C
	ndex = INDEX  ( carr (1), 'T' )
	IF   ( ndex .ne. 0 )  THEN
	    nlun = nlun + 1
	    lun  ( nlun ) = 6
	    devs ( nlun ) = 'T'
	END IF
C
C*	Check for output file requested.
C
	ndex = INDEX  ( carr (1), 'F' )
	IF  ( ndex .ne. 0 )  THEN
	    IF  ( carr (2) .eq. ' ' )  THEN
		CALL ST_LSTR  ( name, lens, ier )
		filnam = name ( : lens ) // '.FIL'
	      ELSE
		filnam = carr (2)
	    END IF
	    IF  ( ( lunf .gt. 0 ) .and. ( carr (2) .ne. ' ' ) .and.
     +		  ( filnam .ne. outfil ) )  THEN
		CALL FL_CLOS  ( lunf, ier )
		lunf   = 0
		outfil = ' '
	    END IF
	    IF  ( lunf .eq. 0 )  THEN
		CALL FL_SWOP  ( filnam, lunf, ier )
		IF  ( ier .ne. 0 )  THEN
		    CALL ER_WMSG  ( 'FL', ier, filnam, ierr )
		    iret = -1
		  ELSE
		    outfil = filnam
		END IF
	    END IF
	    IF  ( lunf .gt. 0 )  THEN
		nlun = nlun + 1
		lun  ( nlun ) = lunf
		devs ( nlun ) = 'F'
	    END IF
	  ELSE IF  ( lunf .gt. 0 )  THEN
	    CALL FL_CLOS  ( lunf, ier )
	    lunf = 0
	END IF
C
C*	If no valid devices were selected, send output to terminal.
C
	IF  ( nlun .eq. 0 )  THEN
	    nlun = nlun + 1
	    lun  ( nlun ) = 6
	    devs ( nlun ) = 'T'
	END IF
C*
	RETURN
	END
