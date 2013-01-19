	SUBROUTINE SF_QDAT  ( isffln, datflg, iret )
C************************************************************************
C* SF_QDAT								*
C*									*
C* This subroutine sets a flag indicating whether data for the current	*
C* station and time are stored in a file.				*
C*									*
C* SF_QDAT  ( ISFFLN, DATFLG, IRET )					*
C*									*
C* Input parameters:							*
C*	ISFFLN		INTEGER		Surface file number		*
C*									*
C* Output parameters:							*
C*	DATFLG		LOGICAL		Data present flag		*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C*					  -3 = file not open		*
C*					  -7 = location not set		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 7/87						*
C* M. desJardins/GSFC	 6/88	Documentation				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'sfcmn.cmn'
C*
	LOGICAL		datflg
C------------------------------------------------------------------------	
	datflg = .false.
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
	CALL DM_QDAT  ( isffln, irow, icol, 'SFDT', datflg, iret )
C*
	RETURN
	END
	
