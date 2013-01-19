	SUBROUTINE SF_CTIM  ( isffln, idate, itime, timflg, iret )
C************************************************************************
C* SF_CTIM								*
C*									*
C* This subroutine checks for the specified date/time in the surface	*
C* file.  It is called internally by the SF routines.			*
C*									*
C* SF_CTIM  ( ISFFLN, IDATE, ITIME, TIMFLG, IRET )			*
C*									*
C* Input parameters:							*
C*	ISFFLN		INTEGER	 	Surface file number		*
C*	IDATE		INTEGER		Date (YYMMDD)			*
C*	ITIME		INTEGER		Time (HHMM)			*
C*									*
C* Output parameters:							*
C*	TIMFLG		LOGICAL		Time found flag			*
C*	IRET		INTEGER		Return code			*
C*				    	   0 = normal return		*
C*				   	  -3 = file not open		*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/87						*
C* M. desJardins/GSFC	 6/88	Documentation				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sfcmn.cmn'
C*
	LOGICAL		timflg
C*
	INTEGER		keyloc (2), keyval (2)
C------------------------------------------------------------------------
C*	Search for row or column.
C
	keyloc (1) = kdate (isffln)
	keyloc (2) = ktime (isffln)
	keyval (1) = idate
	keyval (2) = itime
	CALL DM_SRCH  ( isffln, dttype (isffln), 2, keyloc, keyval,
     +			irwcl, iret )
	IF  ( iret .ne. 0 )  THEN
	    iret = 0
	    timflg = .false.
	  ELSE
	    timflg = .true.
	END IF
C*
	RETURN
	END

