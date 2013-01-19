	SUBROUTINE JB_NEXT ( ifn, iymdh, iret )
C************************************************************************
C* JB_NEXT								*
C*									*
C* This subroutine searchs for the next BUFR report.  If none is	*
C* found, iret = -1.							*
C*									*
C* JB_NEXT  ( IFN, IYMDH, IRET )					*
C*									*
C* Input parameter:							*
C*	IFN		INTEGER		BUFR file number		*
C*									*
C* Output parameters:							*
C*	IYMDH		INTEGER		YYMMDDHH date time		*
C* 	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = END OF FILE		*
C*					 -2 = READ ERROR		*
C**									*
C* Log:									*
C* K. Brill/EMC		10/96						*
C* K. Brill/EMC		 2/97	Save YMDH info; loop while ier != 0	*
C* K. Brill/EMC		 4/97	Check for subset name			*
C************************************************************************
	CHARACTER	subset*8
	SAVE		jymdh
C*
	CHARACTER*8	msgnam
	COMMON		/SUB/ msgnam
C-----------------------------------------------------------------------
	iret = 0
	ier = 1
	DO WHILE ( ier .ne. 0 )
	    CALL READSB ( ifn, ier )
	    IF ( ier .ne. 0 ) THEN
		CALL READMG ( ifn, subset, iymdh, iret )
		DO WHILE ( subset .ne. msgnam .and. msgnam .ne. ' '
     +			   .and. iret .eq. 0 )
		    CALL READMG ( ifn, subset, iymdh, iret )
		END DO
		jymdh = iymdh
		IF ( iret .eq. -1 ) THEN
		    RETURN
		ELSE IF ( iret .ne. 0 ) THEN
		    iret = -2
		    RETURN
		END IF
	    ELSE
		iymdh = jymdh
	    END IF
	END DO
C*
	RETURN
C*
	END
