	SUBROUTINE SI_GTIM  ( istarr, bultim, vmdy, vhh, dattim, vtime, 
     +			      ihhmm, iret )
C************************************************************************
C* SI_GTIM								*
C*									*
C* This subroutine combines an integer system time, the bulletin	*
C* time containing the day, hour and minute, and the verification 	*
C* time into a file and verification times.  It is assumed that the	*
C* system time accurately reflects the year and month of the 		*
C* observation and is later than that time.				*
C*									*
C* SI_GTIM  ( ISTARR, BULTIM, VMDY, VHH, DATTIM, VTIME, IHHMM, IRET )	*
C*									*
C* Input parameters:							*
C*	ISTARR (5)	INTEGER		System time			*
C*	BULTIM		CHAR*		Bulletin day, hour, minute	*
C*	VMDY		CHAR*		Verification mm/dd/yy		*
C*	VHH		CHAR*		Verification hour		*
C*									*
C* Output parameters:							*
C*	DATTIME		CHAR*		File time			*
C*	VTIME		CHAR*		Verification date/time		*
C*	IHHMM		INTEGER		Hour/minute			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -6 = invalid time		*
C**									*
C* Log:									*
C* T. Lee/SAIC		 9/02						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	INTEGER		istarr (*)
	CHARACTER*(*)	bultim, vmdy, vhh, dattim, vtime
C*
	INTEGER		itarr (5)
	EQUIVALENCE	( itarr (1), iyear ), ( itarr (2), imnth ),
     +			( itarr (3), iday  ), ( itarr (4), ihour ),
     +			( itarr (5), imin  )
C------------------------------------------------------------------------
	iret   = 0
C
C*	Use the system time for the observation time for the year and 
C*	month.
C
	iyear = istarr (1)
	imnth = istarr (2)
C
C*	If no bulletin time was sent, use system time for file time.
C
	IF  ( bultim .eq. ' ' )  THEN
	    iday  = istarr (3)
	    CALL TI_ITOC  ( istarr, dattim, ierr )
	    IF  ( ierr .ne. 0 )  iret = -6
	  ELSE
C
C*	    Get the day, hour and minute from the bulletin.
C
	    CALL ST_INTG  ( bultim (1:2), iday,   ier )
	    CALL ST_INTG  ( bultim (3:4), ihour, ier )
	    CALL ST_INTG  ( bultim (5:6), imin,  ier )
C
C*	    Convert to GEMPAK time.
C
	    CALL TI_ITOC  ( itarr, dattim, ierr )
	    IF ( ierr .ne. 0 ) iret = -6
	END IF
C
C*	Get verification time.
C
	CALL ST_INTG ( vmdy (1:2), imnth, ier )
	CALL ST_INTG ( vmdy (4:5), iday, ier )
	CALL ST_INTG ( vmdy (7:8), iyear, ier )
	CALL ST_INTG ( vhh (1:2), ihour, ier )
	CALL ST_INTG ( vhh (3:4), imin, ier )
C
C*	Convert to GEMPAK time.
C
	CALL TI_ITOC ( itarr, vtime, ierr )
	ihhmm = ihour * 100 + imin
	IF ( ierr .ne. 0 ) iret = -6
C*
	RETURN
	END
