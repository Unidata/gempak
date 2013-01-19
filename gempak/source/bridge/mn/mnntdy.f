	SUBROUTINE MN_NTDY ( report, istart, iprm1, iprm2, rdata, iret ) 
C************************************************************************
C* MN_NTDY								*
C*									*
C* This subroutine decodes a line of the NGM MOS report that has	*
C* night and day numerical data encoded as blocks of 3 characters.	*
C* After the strings are parsed, they are converted to reals.		*
C*									*
C* MN_NTDY ( REPORT, ISTART, IPRM1, IPRM2, RDATA, IRET )			*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		NGM MOS report			*
C*	ISTART		INTEGER		Index of line beginnings	*
C*	IPRM1		INTEGER		Position of first parm in list	*
C*	IPRM2		INTEGER		Position of second parm in list	*
C*									*
C* Output parameters:							*
C*	RDATA		REAL		Forecast data			*
C*	(FCSTTM,MMPARM)							*
C*	IRET		INTEGER		Return Code			*
C**									*
C* Log:									*
C* F. J. Yen		11/98		Created				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INTEGER		FCSTTM
	PARAMETER       ( FCSTTM = 19 )
C*
	CHARACTER*(*)	report
	REAL		rdata(FCSTTM,MMPARM)
C
	CHARACTER	tstr*3
C------------------------------------------------------------------------
	iret = 0
C
C*	Parse the specified line of the report.
C
	IF ( istart .ne. 0 ) THEN
	    ibeg = istart + 18
	    iend = INDEX ( report(istart:), CHCR ) + istart
	    incr = 2
	    iprm = iprm1
	    DO i = 7, FCSTTM, 4
	 
		IF ( ibeg .lt. iend ) THEN
		    tstr = report (ibeg:ibeg+incr)
C
C*		    Convert each string to a real.
C
		    CALL ST_CRNM ( tstr, rdata(i,iprm), ier )
		    IF ( iprm .eq. iprm1 ) THEN
			iprm = iprm2
		      ELSE
			iprm = iprm1
		    END IF
	            ibeg = ibeg + 12	

		END IF
		
	    END DO
	END IF
C*
	RETURN
	END
