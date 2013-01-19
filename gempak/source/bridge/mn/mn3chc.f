	SUBROUTINE MN_3CHC ( report, istart, iprms, rdata, iret ) 
C************************************************************************
C* MN_3CHC								*
C*									*
C* This subroutine decodes a line of the NGM MOS report that has 1-, 2-,*
C* or 3- letter codes. After the strings are parsed, they are converted	*
C* to reals. 								*
C*									*
C* MN_3CHC ( REPORT, ISTART, IPRMS, RDATA, IRET )			*
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
C* F. J. Yen/NCEP	11/98	Converted for restructuring of DC_NMDC	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INTEGER		FCSTTM
	PARAMETER	( FCSTTM = 19 )
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
C*		    If the CLDS line, convert to SAO code.
C
		    IF ( iprms .eq. 5 ) THEN
		    	IF ( tstr .eq. ' CL' ) tstr = 'CLR'
		    	IF ( tstr .eq. ' SC' ) tstr = 'SCT' 
		    	IF ( tstr .eq. ' BK' ) tstr = 'BKN' 
		    	IF ( tstr .eq. ' OV' ) tstr = 'OVC'
			rdata(i,iprms) = PT_CCNM ( tstr )	
		    END IF
C
C*		    PTYPE line.
C
		    IF ( iprms .eq. 16 ) THEN
			IF ( tstr .eq. '  R' ) rdata(i,iprms) = 0.0
			IF ( tstr .eq. '  S' ) rdata(i,iprms) = 1.0
			IF ( tstr .eq. '  Z' ) rdata(i,iprms) = 2.0
		    END IF
C
C*		    OBVIS line.
C
		    IF ( iprms .eq. 23 ) THEN
			IF ( tstr .eq. '  N' ) rdata(i,iprms) = 0.0
			IF ( tstr .eq. '  F' ) rdata(i,iprms) = 1.0
			IF ( tstr .eq. '  H' ) rdata(i,iprms) = 2.0
		    END IF
		    ibeg = ibeg + incr + 1	
		END IF
	    END DO
	END IF
C*
	RETURN
	END
