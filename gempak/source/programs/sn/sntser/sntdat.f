	SUBROUTINE SNTDAT  ( iflno, dattim, newfil, datcur,
     +			     npts,  timfnd, ctime,  iret )
C************************************************************************
C* SNTDAT								*
C*									*
C* This subroutine generates a list of times from the user's date/time	*
C* input.								*
C*									*
C* SNTDAT  ( IFLNO, DATTIM, NEWFIL, DATCUR, NPTS, TIMFND,	*
C*           CTIME, IRET )						*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		Sounding file number		*
C*	DATTIM		CHAR*		Date/time input			*
C*	NEWFIL		LOGICAL		New file flag			*
C*									*
C* Input and output parameters:						*
C*	DATCUR		CHAR*		Current date/time		*
C*									*
C* Output parameters:							*
C*	NPTS		INTEGER		Number of times 		*
C*	TIMFND (NPTS)	CHAR*		Times 				*
C*	CTIME		CHAR*		Actual date/time range		*
C*	IRET		INTEGER		Return code			*
C*					  1 = user typed EXIT		*
C*					  0 = normal			*
C*					 -9 = no points found to plot	*
C*					-12 = invalid time		*
C*					 -7 = session not interactive	*
C**									*
C* Log:									*
C* G. Huffman/USRA	 5/89	Adapted from SNLDAT			*
C* S. Jacobs/NCEP	 1/99	Removed respnd; Changed to use TI_FIND	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)   datcur, dattim, timfnd (*), ctime
	LOGICAL         newfil
C*
	CHARACTER	timfil (LLMXTM)*20, time*20,
     +			tstrt*20, tstop*20, tout*15
C------------------------------------------------------------------------
	iret = 0
C
C*	Check for current time equal to the new time request.
C
	CALL ST_LCUC  ( dattim, time, ier )
	IF  ( ( time .ne. datcur ) .or. ( time .eq. 'LIST' ) .or.
     +	      ( newfil ) .or. ( dattim .eq. ' ' ) )  THEN
	    datcur = ' '
C
C*	    Get times from file.
C
	    CALL SN_GTIM  ( iflno, LLMXTM, ntimf, timfil, ier )
	    IF  ( ier .ne. 0 )  THEN
	        CALL ER_WMSG  ( 'SN', ier, ' ', ierr )
		iret   = -12
	        RETURN
	    END IF
C
C*	    Find the time.
C
	    CALL TI_FIND  ( dattim, ntimf, timfil, tout, npts,
     +			    timfnd, ier )
	    IF  ( ier .eq. 0 )  THEN
		datcur = dattim
	      ELSE
		iret   = -12
	    END IF
C
C*	    Encode the times . . . reduce the stop time to DD/HH if
C*	    if YY and MM are the same and minutes are zero.
C
            tstrt = timfnd (1)
            tstop = timfnd (npts)
            IF  ( ( tstrt ( 10:11 ) .eq. '00' ) .and.
     +            ( tstop ( 10:11 ) .eq. '00' ) )  THEN
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
