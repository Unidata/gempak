	SUBROUTINE OACSTN  ( parms, nparms, stns, nqc, qcntl, iret )
C************************************************************************
C* OACSTN								*
C*									*
C* This subroutine writes stations which are discarded by QC to 	*
C* terminal for	OABSFC.							*
C*									*
C* OACSTN  ( PARMS, NPARMS, STNS, NQC, QCNTL, IRET )			*
C*									*
C* Input parameters:							*
C*	PARMS (NPARMS)	CHAR*		Parameters			*
C*	NPARMS		INTEGER		Number of parameters		*
C*	STNS		CHAR*		Station ID			*
C*	 (NPARMS,LLSTFL*MMFILE)						*
C*	NQC (NPARMS,2)	INTEGER		Number of stations		*
C*	QCNTL (NPARMS)	REAL		QC threshold			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* T. Lee/GSC	 	3/99						*
C************************************************************************
	CHARACTER*(*)	parms ( * ), stns ( NPARMS, * )
	INTEGER		nqc ( NPARMS, 2 )
	REAL		qcntl ( NPARMS )
C------------------------------------------------------------------------
	iret = 0
C
C*      Write station ID to terminal.
C
	DO i = 1, nparms
	    IF  ( qcntl (i) .ne. 0. )  THEN
C
C*		Write out parameter.
C
		WRITE ( 6, 1010, IOSTAT = iostat ) parms ( i )
1010		FORMAT ( / ' PARAMETER: ', 2X, A4 )
C
C*		Write out # of stations discarded by QC.
C
		WRITE ( 6, 1020, IOSTAT = iostat ) nqc ( i, 1 )
1020		FORMAT ( 1X, I4, ' stations discarded by QC ')
C
C*		Write out station IDs.
C
		IF  ( nqc ( i, 2 ) .gt. 0 )  THEN
		    WRITE (6, 1030, IOSTAT = iostat )
     +			  ( stns ( i, j ), j = 1, nqc ( i, 2 ) )
1030		    FORMAT ( ( 3X, 8 ( A, 1X ) ) ) 
		END IF
	    END IF
	END DO
C*
	RETURN
	END
