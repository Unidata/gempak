	SUBROUTINE SF_QTXT  ( isffln, txtflg, iret )
C************************************************************************
C* SF_QTXT								*
C*									*
C* This subroutine sets a flag indicating whether text reports for	*
C* the current station and time are stored in a file.			*
C*									*
C* SF_QTXT  ( ISFFLN, TXTFLG, IRET )					*
C*									*
C* Input parameters:							*
C*	ISFFLN		INTEGER		Surface file number		*
C*									*
C* Output parameters:							*
C*	TXTFLG		LOGICAL		Text present flag		*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C*					  -3 = file not open		*
C*					  -7 = location not set		*
C**									*
C* Log:									*
C* K. Tyle/GSC		 5/97	Based on SF_QDAT			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'sfcmn.cmn'
C*
	LOGICAL		txtflg
C------------------------------------------------------------------------	
	txtflg = .false.
C
C*	Check that file is open.
C
	CALL SF_CHKF  ( isffln, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Check that station is set.
C
	irow = krow (isffln)
	icol = kcol (isffln)
	IF  ( ( irow .eq. 0 ) .or. ( icol .eq. 0 ) )  THEN
	    iret = -7
	    RETURN
	END IF
C
C*	Query for data.
C
	CALL DM_QDAT  ( isffln, irow, icol, 'SFTX', txtflg, iret )
C*
	RETURN
	END
	
