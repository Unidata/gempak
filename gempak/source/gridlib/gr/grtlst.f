	SUBROUTINE GR_TLST  ( gdatim, igdfln, ntime, timfnd, iret )
C************************************************************************
C* GR_TLST								*
C*									*
C* This subroutine gets a list of times to be input to the grid		*
C* programs.								*
C*									*
C* GR_TLST  ( GDATIM, IGDFLN, NTIME, TIMFND, IRET )			*
C*									*
C* Input parameters:							*
C*	GDATIM		CHAR*		Input grid time list		*
C*	IGDFLN		INTEGER		Grid file number		*
C*									*
C* Output parameters:							*
C*	NTIME		INTEGER		Number of times			*
C*	TIMFND (NTIME)	CHAR*		Times				*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-20 = no times selected		*
C**									*
C* Log:									*
C* P. Bruehl/Unidata	 8/94	Subroutine in GDCNTR			*
C* M. desJardins/NMC	 8/94	Moved to subroutine			*
C* S. Jacobs/NCEP	11/96	Removed check for MXLOOP number of times*
C* T. Lee/GSC		 9/98	Increased TIMLST dimension to LLMXGT	*
C* D.W.Plummer/NCEP	12/98	Changed LLMXTM to LLMXGT		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	gdatim, timfnd (*)
C*
	CHARACTER*20	timlst (LLMXGT), timout*40
C------------------------------------------------------------------------
	iret = 0
C
C*	If there is a grid with two times (i.e. containing a ':'),
C*	assume that the user has input a list of times.
C
	itwotm = INDEX ( gdatim, ':')
	IF  ( itwotm .gt. 0 )  THEN
C
C*	    Break input line into list of times.
C
	    CALL ST_CLST  ( gdatim, ';', ' ', LLMXGT, timfnd, ntime,
     +			    iret )
	  ELSE
C
C*	    Get list of times in data file.
C
	    CALL GD_GTIM ( igdfln, LLMXGT, timlst, ntimin, ier )
	    IF  ( ntimin .eq. 0 )  THEN
		iret = -4
		CALL ER_WMSG ( 'TG', iret, ' ', ier )
		iret = -20
	    END IF
C
C*	    Call TG_FIND to get list of times specified in gdatim
C
	    CALL TG_FIND ( gdatim, ntimin, timlst, timout, ntime,
     +			   timfnd, ier )
	    IF ( ( ier .ne. 0 ) .or. ( ntime .eq. 0 ) )  THEN
		iret = -20
		CALL ER_WMSG ( 'GR', iret, ' ', ier )
	    END IF
	END IF
C*
	RETURN
	END
