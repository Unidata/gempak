	SUBROUTINE RA_CHCK  ( isffln, dattim, stid, timflg, stnflg,
     +			      datflg, iret )
C************************************************************************
C* RA_CHCK								*
C*									*
C* This subroutine checks to see if a time and station are already in	*
C* the surface file.  If the time and/or station is not found, the	*
C* logical variables TIMFLG and/or STNFLG is set to false.  If both	*
C* the time and station are found, datflg will be true if data for	*
C* this station has already been added to the file.			*
C*									*
C* RA_CHCK  ( ISFFLN, DATTIM, STID, TIMFLG, STNFLG, DATFLG, IRET )	*
C*									*
C* Input parameters:							*
C*	ISFFLN		INTEGER		Sounding file number		*
C*	DATTIM		CHAR*15		Nominal date/time		*
C*	STID		CHAR*		Station identifier		*
C*									*
C* Output parameters:							*
C*	TIMFLG		LOGICAL		Time found flag			*
C*	STNFLG		LOGICAL		Station found flag		*
C*	DATFLG		LOGICAL		Data already in file flag	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/90	GEMPAK 5				*
C* J. Whistler/SSAI	 3/91	Changed STID from a *4 to a *(*)	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	dattim, stid
	LOGICAL		timflg, stnflg, datflg
C*
C------------------------------------------------------------------------
	iret   = 0
	datflg = .false.
	timflg = .false.
	stnflg = .false.
C
C*	Find the time in the file.
C
	CALL SF_FTIM  ( isffln, dattim, iret )
	IF  ( iret .eq. 0 )  timflg = .true.
C
C*	Set the station.
C
	CALL SF_FSTN  ( isffln, stid, iret )
	IF  ( iret .eq. 0 )  stnflg = .true.
C
C*	If the time and station are both found, check for data.
C
	IF  ( timflg .and. stnflg )  THEN
	    CALL SF_QDAT  ( isffln, datflg, ier )
	END IF
C*
	RETURN
	END
