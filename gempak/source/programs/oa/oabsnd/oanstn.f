	SUBROUTINE OANSTN  ( gparm, ngrid, iglev, stns, nqc, qcntl, 
     +			     iret )
C************************************************************************
C* OANSTN								*
C*									*
C* This subroutine writes stations which are discarded by QC to 	*
C* terminal for OABSND.							*
C*									*
C* OANSTN  ( GPARM, NGRID, IGLEV, STNS, NQC, QCNTL, IRET )		*
C*									*
C* Input parameters:							*
C*	GPARM (NGRID)	CHAR*		Parameters			*
C*	NGRID		INTEGER		Number of grids 		*
C*	IGLEV (NGRID)	INTEGER		Grid level			*
C*	STNS		CHAR*		Station ID			*
C*	 (NGRID,LLSTFL*MMFILE)						*
C*	NQC (NGRID,2)	INTEGER		Number of stations		*
C*	QCNTL (NGRID)	REAL		QC threshold			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* T. Lee/GSC	 	3/99						*
C************************************************************************
	CHARACTER*(*)	gparm ( * ), stns ( NGRID, * )
	INTEGER		iglev ( * ), nqc ( NGRID, 2 )
	REAL		qcntl ( NGRID )
C------------------------------------------------------------------------
	iret = 0
C
C*      Write station ID to terminal.
C
	levold = -99999
	DO i = 1, ngrid
	    IF  ( qcntl (i) .ne. 0. )  THEN	
C
C*		Write out level.
C
		IF  ( iglev (i) .ne. levold )  THEN
		    WRITE  ( 6, 1000, IOSTAT = iostat ) iglev (i)
1000		    FORMAT ( / ' LEVEL: ', I8 )
		    levold = iglev ( i )
		END IF
C
C*		Write out parameter.
C
		WRITE  ( 6, 1010, IOSTAT = iostat ) gparm (i)
1010		FORMAT ( 2X, ' PARAMETER: ', 2X, A4 )
C
C*		Write out # of stations discarded by QC.
C
		WRITE  ( 6, 1020, IOSTAT = iostat ) nqc ( i, 1 )
1020		FORMAT ( 1X, I4, ' stations discarded by QC ' )
C
C*		Write out station IDs.
C
		IF  ( nqc ( i, 2 ) .gt. 0 )  THEN
		    WRITE  ( 6, 1030, IOSTAT = iostat ) 
     +			   ( stns ( i, j ), j = 1, nqc ( i, 2 ) )
1030		    FORMAT ( ( 3X, 8 ( A, 1X ) ) )
		END IF
	    END IF
	END DO
C*
	RETURN
	END
