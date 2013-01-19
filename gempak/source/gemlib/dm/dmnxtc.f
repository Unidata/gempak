	SUBROUTINE DM_NXTC  ( ircpnt, iret )
C************************************************************************
C* DM_NXTC								*
C*									*
C* This subroutine returns the next record to write in the cache.	*
C*									*
C* DM_NXTC  ( IRCPNT, IRET )						*
C*									*
C* Output parameters:							*
C*	IRCPNT		INTEGER		Next record			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -6 = write error		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 3/87						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'dmcmn.cmn'
C-----------------------------------------------------------------------
	iret = 0
C
C*	Get next record number.
C
	kclast = kclast + 1
	IF  ( kclast .gt. MCACHE ) kclast = 1
C
C*	Write old data if necessary.
C
	IF  ( kwrite ( kclast ) ) THEN
	    CALL FL_WRIT  ( lundm (kcflno (kclast) ), kcrecn (kclast),
     +			    MBLKSZ, kcdata (1,kclast), iflerr )
	    kwrite ( kclast ) = .false.
	    IF  ( iflerr .ne. 0 ) THEN
		CALL ER_WMSG ( 'FL', iflerr, ' ', ier )
		iret = -6
	    END IF
	END IF
C
C*	Return record number.
C
	ircpnt = kclast
C*
	RETURN
	END
