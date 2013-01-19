	SUBROUTINE TG_VI2F  ( vdtm, idtm, fdtm, lnth, iret )
C************************************************************************
C* DE_VI2F								*
C*									*
C* This subroutine converts a forecast valid time of the form 		*
C* YYMMDD/HHNN and an initial GEMPAK time of the form yymmdd/hhnn 	*
C* into a proper GEMPAK forecast time stamp of the form			*
C* yymmdd/hhnnFhhhnn.							*
C*									*
C* TG_VI2F  ( VDTM, IDTM, FDTM, LNTH, IRET )				*
C*									*
C* Input parameters:							*
C*	VDTM		CHAR*		Valid GEMPAK time, YYMMDD/HHNN	* 
C*	IDTM		CHAR*		Init. GEMPAK time, yymmdd/hhnn	* 
C*									*
C* Output parameters:							*
C*	FDTM		CHAR*		Forecast time, yymmdd/hhnn	*
C*	LNTH		INTEGER		Length of string FDTM		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = invalid date or time	*
C*					 -3 = invalid forecast time	*
C**									*
C* Log:									*
C* T. Lee/SAIC		 1/05						*
C************************************************************************
	CHARACTER*(*)	vdtm, idtm, fdtm
	CHARACTER	sfh*3, snn*2	
C------------------------------------------------------------------------
	iret = 0
	lnth = 0
C
C*	Return if VDTM or IDTM is blank,
C
	IF ( ( vdtm .eq. ' ' ) .or. ( idtm .eq. ' ' ) )  THEN
	    iret = -1
	    fdtm = ' '
	    RETURN
	END IF
C
C*	Compute the difference between valid time and initial time.
C
	CALL TG_DIFF ( vdtm, idtm, nmin, iret )
	IF ( iret .ne. 0 )  RETURN
C
	IF ( ( nmin .lt. 0 ) .or. ( nmin .ge. 60000 ) )  THEN
	   iret = -3
	   RETURN
	END IF
C
	ifh = nmin / 60 
	nn = MOD ( nmin, 60 )
	WRITE ( sfh, 50, IOSTAT = ier ) ifh
50	FORMAT ( I3.3 )
	WRITE ( snn, 60, IOSTAT = ier ) nn
60	FORMAT ( I2.2 )
C
C*	Construct the forecast time stamp.
C
	CALL ST_LSTR ( idtm, lstr, ier )
	fdtm = idtm ( :lstr ) // 'F' // sfh // snn
C
	CALL ST_LSTR ( fdtm, lnth, ier )
C*	
	RETURN
	END
