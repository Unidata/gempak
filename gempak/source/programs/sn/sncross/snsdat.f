	SUBROUTINE SNSDAT  ( isnfln, dattim, datold, newdat, times, 
     +			     ntime, iret )
C************************************************************************
C* SNSDAT								*
C*									*
C* This subroutine gets the times for the cross section program.	*
C* If one time is entered, the program will draw a cross section.	*
C* If more than one time is entered, the program will draw a time	*
C* section.								*
C*									*
C* SNSDAT  ( ISNFLN, DATTIM, DATOLD, NEWDAT, TIMES, NTIME, IRET )	*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER		File number			*
C*	DATTIM		CHAR*		User input date/time		*
C*									*
C* Input and output parameters:						*
C*	DATOLD		CHAR*		Current date/time input		*
C*	NEWDAT		LOGICAL		New data selected flag		*
C*									*
C* Output parameters:							*
C*	TIMES (NTIME)	CHAR*		GEMPAK times			*
C*	NTIME		INTEGER		Number of times selected	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -5 = invalid time		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/90	GEMPAK 5				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	dattim, times (*), datold
	LOGICAL		newdat
C*
	CHARACTER	dstime (LLMXTM)*20, timout*20
C------------------------------------------------------------------------
	iret = 0
C
C*	Check for the same time.
C
	IF  ( ( .not. newdat ) .and. ( dattim .eq. datold ) .and.
     +	      ( datold .ne. ' ' ) )  THEN
	    RETURN
	END IF
C
C*	Read in the full list of times from the file.
C
	newdat = .true.
	datold = ' '
	CALL SN_GTIM  ( isnfln, LLMXTM, ntimf, dstime, ier )
	IF  ( ier .ne. 0 )  THEN
	    iret = -5
	    CALL ER_WMSG  ( 'SN', ier , ' ', ier2 )
	    RETURN
	  ELSE IF  ( ntimf .eq. 0 )  THEN
	    iret = -5
	    CALL ER_WMSG  ( 'SNCROSS', iret, ' ', ier )
	    RETURN
	END IF
C
C*	Find the requested time.
C
	CALL TI_FIND  ( dattim, ntimf, dstime, timout, ntime, times,
     +			ier )
	IF  ( ier .ne. 0 )  THEN
	    iret = -5
	    RETURN
	  ELSE
	    datold = dattim
	END IF
C
C*	Set the time if there is only one time.
C

	IF  ( ntime .eq. 1 )  THEN
	    CALL SN_STIM  ( isnfln, times (1), ier )
	    IF  ( ier .ne. 0 )  THEN
		CALL ER_WMSG ( 'SN', ier, ' ', ier2 )
		iret   = -5
		datold = ' '
	    END IF
	END IF
C*
	RETURN
	END
