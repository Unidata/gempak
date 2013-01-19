	SUBROUTINE SN_QDAT  ( isnfln, datflg, iret )
C************************************************************************
C* SN_QDAT								*
C*									*
C* This subroutine sets a flag indicating whether data for the 		*
C* current station and time are stored in a file.  If the data		*
C* are not merged, only the mandatory below and above 100 mb and	*
C* the significant temperature below 100 mb data are checked.		*
C*									*
C* SN_QDAT  ( ISNFLN, DATFLG, IRET )					*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER		Sounding file number		*
C*									*
C* Output parameters:							*
C*	DATFLG		LOGICAL		Data present flag		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not open		*
C*					 -8 = location not set		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	7/87						*
C* D. Kidwell/NCEP	2/01	Added PPAA and PPCC                     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'sncmn.cmn'
C*
	LOGICAL		datflg
C------------------------------------------------------------------------	
	datflg = .false.
C
C*	Check that file is open.
C
	CALL SN_CHKF ( isnfln, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Check that row and column are set.
C
	irow = krow ( isnfln )
	icol = kcol ( isnfln )
	IF  ( ( irow .eq. 0 ) .or. ( icol .eq. 0 ) )  THEN
	    iret = -8
	    RETURN
	END IF
C
C*	Query for data.
C
	IF  ( mrgtyp ( isnfln ) )  THEN
	    CALL DM_QDAT  ( isnfln, irow, icol, 'SNDT', datflg, ier )
	  ELSE
	    IF  ( taflg ( isnfln ) )  THEN
		CALL DM_QDAT ( isnfln, irow, icol, 'TTAA', datflg, ier )
	    END IF
	    IF  ( tcflg ( isnfln )  .and.  ( .not. datflg ) )  THEN
		CALL DM_QDAT ( isnfln, irow, icol, 'TTCC', datflg, ier )
	    END IF
	    IF  ( tbflg ( isnfln )  .and.  ( .not. datflg ) )  THEN
		CALL DM_QDAT ( isnfln, irow, icol, 'TTBB', datflg, ier )
	    END IF
	    IF  ( paflg ( isnfln )  .and.  ( .not. datflg ) )  THEN
		CALL DM_QDAT ( isnfln, irow, icol, 'PPAA', datflg, ier )
	    END IF
	    IF  ( pcflg ( isnfln )  .and.  ( .not. datflg ) )  THEN
		CALL DM_QDAT ( isnfln, irow, icol, 'PPCC', datflg, ier )
	    END IF
	END IF
C*
	RETURN
	END
