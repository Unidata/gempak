	SUBROUTINE SN_CTIM ( isnfln, idate, itime, timflg, iret )
C************************************************************************
C* SN_CTIM								*
C*									*
C* This subroutine determines if the specified date/time exists in	*
C* the sounding file.							*
C*									*
C* SN_CTIM ( ISNFLN, IDATE, ITIME, TIMFLG, IRET )			*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER	 	Sounding file number		*
C*	IDATE		INTEGER		Date (YYMMDD)			*
C*	ITIME		INTEGER		Time (HHMM)			*
C*									*
C* Output parameters:							*
C*	TIMFLG		LOGICAL		Time found flag			*
C*	IRET		INTEGER		Return code			*
C*				    	   0 = normal return		*
C*				   	  -4 = file not open		*
C**									*
C* Log:									*
C* I. Graffman/RDS	5/87						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sncmn.cmn'
C*
	INTEGER		keyloc (2), keyval (2)
	LOGICAL		timflg
	DATA		nkey /2/
C------------------------------------------------------------------------
C*	Set up search for time.
C
	keyloc (1) = kdate (isnfln)
	keyloc (2) = ktime (isnfln)
	keyval (1) = idate
	keyval (2) = itime
C
C*	Do search and check error.
C
	CALL DM_SRCH ( isnfln, dttype (isnfln), nkey, keyloc, keyval,
     +			irwcl, iret )
	IF (iret .ne. 0) THEN
	    iret = 0
	    timflg = .false.
	  ELSE
	    timflg = .true.
	END IF
C*
	RETURN
	END

