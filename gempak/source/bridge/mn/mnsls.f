	SUBROUTINE MN_SLS ( report, istart, iprmsl, iprmsr, incr,
     +			     rdata, iret ) 
C************************************************************************
C* MN_SLS								*
C*									*
C* This subroutine decodes a line of the NGM MOS report that has pairs	*
C* of numerical data encoded as blocks of 3 characters. The numbers in	*
C* each pair are separated by a slash '/'. After the strings are parsed,*
C* they are coverted to reals.						*
C*									*
C* MN_SLS ( REPORT, ISTART, IPRMSL, IPRMSR, INCR, RDATA, IRET )		*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		NGM MOS report			*
C*	ISTART		INTEGER		Indicies of line beginnings	*
C*	IPRMSL		INTEGER		Position of number left of '/'	*
C*	IPRMSR		INTEGER		Position of number right of '/'	*
C*	INCR		INTEGER		Increment between values in line*
C*									*
C* Output parameters:							*
C*	RDATA		REAL		Forecast data			*
C*	(FCSTTM,MMPARM)							*
C*	IRET		INTEGER		Return Code			*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	11/98	Combined MN_3SLS and MN_6SLS		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INTEGER		FCSTTM
	PARAMETER       ( FCSTTM = 19 )
C*
	CHARACTER*(*)	report
	REAL		rdata(FCSTTM,MMPARM)
C
	CHARACTER	tstr*6
	REAL		rtmp(2)
C------------------------------------------------------------------------
	iret = 0
	
	IF ( incr .eq. 2 ) THEN
	    ioffset = 0
	    idost = 1
	    idoinc = 1
	  ELSE IF (incr .eq. 5 ) THEN
	    ioffset = 3
	    idost = 3
	    idoinc = 2
	END IF
C
C*	Parse the specified line of the report.
C
	IF ( istart .ne. 0 ) THEN
	    ibeg = istart + ioffset
	    iend = INDEX ( report(istart:), CHCR ) + istart
	    DO i = idost, FCSTTM, idoinc 
		IF ( ibeg .lt. iend ) THEN
		    tstr = report (ibeg:ibeg+incr)
C
C*		    Parse each string with a '/', then convert to real.
C
		    IF ( tstr .ne. ' ' ) THEN
			CALL ST_RLST ( tstr, '/', RMISSD, 2, rtmp, 
     +				       num, ier )
			rdata(i,iprmsl) = rtmp(1)
			rdata(i,iprmsr) = rtmp(2)
		    END IF
		    ibeg = ibeg + incr + 1	
		END IF
	    END DO
	END IF
C*
	RETURN
	END
