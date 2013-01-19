	SUBROUTINE MN_3CHR ( report, istart, iprms, rdata, iret ) 
C************************************************************************
C* MN_3CHR								*
C*									*
C* This subroutine decodes a line of the NGM MOS report that has	*
C* numerical data encoded as blocks of 3 characters.. After the 	*
C* strings are parsed, they are converted to reals.			*
C*									*
C* MN_3CHR ( REPORT, ISTART, IPRMS, RDATA, IRET )			*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		NGM MOS report			*
C*	ISTART		INTEGER		Index of line beginnings	*
C*	IPRMS		INTEGER		Position of parameter in list	*
C*									*
C* Output parameters:							*
C*	RDATA		REAL		Forecast data			*
C*	(FCSTTM,MMPARM)							*
C*	IRET		INTEGER		Return Code			*
C**									*
C* Log:									*
C* D. Keiser/GSC	 2/96						*
C* D. Keiser/GSC	 5/96	Declare FCSTTM an integer		*
C* F. J. Yen/NCEP	11/98	Updated for restructuring of DC_NMDC	*
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
	    ibeg = istart
	    iend = INDEX ( report(istart:), CHCR ) + istart
	    incr = 2
	    DO i = 1, FCSTTM 
		IF ( ibeg .lt. iend ) THEN
		    tstr = report (ibeg:ibeg+incr)
C
C*		    Convert each string to a real.
C
		    CALL ST_CRNM ( tstr, rdata(i,iprms), ier )
C
C*		    If WSPD line, set all 99's to missing values.
C
		    IF ( iprms .eq. 5 ) THEN
			IF ( rdata(i,iprms) .ge. 99.0 ) 
     +					rdata(i,iprms) = RMISSD
		    END IF
C
C*		    If WDIR line, convert to degrees.
C
		    IF ( iprms .eq. 7 ) THEN
			rdata(i,iprms) = rdata(i,iprms) * 10.0
		    END IF
		    ibeg = ibeg + incr + 1	
		END IF
	    END DO
	END IF
C*
	RETURN
	END
