	SUBROUTINE ER_STAT  ( nelevl, nebuff, ntmflg, iret )
C************************************************************************
C* ER_STAT								*
C*									*
C* This subroutine sets the error status level, the output destination,	*
C* and a time flag which will be used for subsequent messages.  If the	*
C* time flag is true, the current time will be prepended to the		*
C* message.								*
C*									*
C* Within GEMPAK, the default error level is 0, the default output is	*
C* 0, and the time flag is false.					*
C*									*
C* Messages from the buffer can be written to the terminal by er_wbuf.	*
C* The GUI programs retrieve the messages directly from the buffer.	*
C*									*
C* ER_STAT  ( NELEVL, NEBUFF, NTMFLG, IRET )				*
C*									*
C* Input parameters:							*
C*	NELEVL		INTEGER		Error level			*
C*					   0 = always log		*
C*					   2 = detailed information	*
C*					   4 = debug information	*
C*	NEBUFF		INTEGER		Output destination		*
C*					  -1 = no output		*
C*					   0 = STDOUT			*
C*					   1 = buffer			*
C*	NTMFLG		LOGICAL		Time flag			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* P. Neilley/NCAR	 7/94	Initial version				*
C* M. desJardins/NMC	 8/94	Changed logical to integer		*
C* K. Tyle/GSC		12/96	Use ERCMN variables			*
C* K. Maxwell/GSC	 6/97	Documentation changes			*
C************************************************************************
	INCLUDE		'ercmn.cmn'
C*
	LOGICAL 	ntmflg
C------------------------------------------------------------------------
	iret = 0
C
C*	Check that the input is valid.
C
	IF  (  nelevl .le. 0 ) THEN
	    ielevl = 0
	  ELSE IF ( nelevl .ge. 4 ) THEN
	    ielevl = 4
	  ELSE 
	    ielevl = nelevl
	END IF
	IF ( ( nebuff .ge. -1 ) .and. ( nebuff .le. 1 ) ) THEN
	    iebuff = nebuff
	  ELSE
	    iebuff = 0
	END IF	
	etmflg = ntmflg
C*
	RETURN
	END
