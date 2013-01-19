	SUBROUTINE SFXDAT  ( dattim, times, ntime, newfil, nts, 
     +			     timfnd, ctime, iret )
C************************************************************************
C* SFXDAT								*
C*									*
C* This subroutine generates a list of times from the user's date/time	*
C* input.								*
C*									*
C* SFXDAT  ( DATTIM, TIMES, NTIME, NEWFIL, NTS, TIMFND,	CTIME, IRET )	*
C*									*
C* Input parameters:							*
C*	DATTIM		CHAR*72		Date/time input			*
C*	TIMES(*)	CHAR*		Array of times			*
C*	NTIME		INTEGER		Number of times			*
C*	NEWFIL		LOGICAL		New file flag			*
C*									*
C* Output parameters:							*
C*	NTS		INTEGER		Number of times 		*
C*	TIMFND (NTS)	CHAR*		Times 				*
C*	CTIME		CHAR*		Actual date/time range		*
C*	IRET		INTEGER		Return code			*
C*					  1 = user typed EXIT		*
C*					  0 = normal return		*
C*					-12 = invalid time		*
C**									*
C* Log:									*
C* G. Huffman/USRA	 6/89						*
C* M. desJardins/GSFC	 5/90	Rewritten				*
C* S. Schotz/GSC	 5/90	Get respnd locally from IP_RESP		*
C* A. Hardy/GSC		 3/99   Increased character length of time      *
C* T. Lee/GSC		 5/01	Retrieved data from multiple files	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)   dattim, times (*), timfnd (*), ctime
	LOGICAL         respnd, newfil
C*
	CHARACTER	tstrt*20, tstop*20, timout*20
C------------------------------------------------------------------------
	iret = 0
C
C*	Check for current time equal to the new time request.
C
	CALL ST_LCUC  ( dattim, dattim, ier )
	IF   (  ( dattim .eq. 'LIST' ) .or. ( newfil ) .or.
     +		( dattim .eq. ' ' ) )   THEN
C
C*	    If DATTIM is 'LIST' and the session is interactive, 
C*	    list the times and get a range.
C
	    IF  ( dattim .eq. 'LIST' )  THEN
C
		CALL IP_RESP ( respnd, ier )
		IF  ( respnd )  THEN
		    CALL TI_DSPL  ( ntime, times, dattim, ier )
		    IF  ( ier .eq. 1 )  THEN
			iret = 1
			RETURN
		      ELSE
			CALL ST_LCUC ( dattim, dattim, ier )
		    END IF
		END IF
	    END IF
C
C*	    Find the time.
C
	    CALL TI_FIND  ( dattim, ntime, times, timout,
     +			    nts, timfnd, ier )
	    IF  ( ier .ne. 0 )  THEN
		iret   = -12
		RETURN
	    END IF
C
C*	    Encode the times . . . reduce the stop time to DD/HH if
C*	    if YY and MM are the same and minutes are zero.
C
	    tstrt = timfnd (1)
	    tstop = timfnd (nts)
	    IF  ( ( tstrt ( 10:11 ) .eq. '00' ) .and.
     +	          ( tstop ( 10:11 ) .eq. '00' ) )  THEN
	        tstrt = tstrt ( 1:9 )
	        tstop = tstop ( 1:9 )
	    END IF
	    CALL ST_LSTR  ( tstrt, lstrt, ier )
	    IF  ( tstrt ( 1:6 ) .eq. tstop ( 1:6 ) )  THEN
	        tstop = tstop ( 7: )
	      ELSE IF  ( tstrt ( 1:4 ) .eq. tstop ( 1:4 ) )  THEN
	        tstop = tstop ( 5: )
	      ELSE IF  ( tstrt ( 1:2 ) .eq. tstop ( 1:2 ) )  THEN
	        tstop = tstop ( 3: )
	    END IF
	    ctime = tstrt ( 1:lstrt ) // '-' // tstop
	END IF
C*
	RETURN
	END
