	SUBROUTINE MS_DCDA ( report, istart, iprms, ifcstm, irhour,
     +			     rdata, iret ) 
C************************************************************************
C* MS_DCDA								*
C*									*
C* This subroutine decodes the data contained in a GFS MOS report.	*
C*									*
C* MS_DCDA ( REPORT, ISTART, IPRMS, IFCSTM, IRHOUR, RDATA, IRET )	*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		GFS MOS report			*
C*	ISTART (*)	INTEGER		Indices of line beginnings	*
C*	IPRMS (*)	INTEGER		Position of parameter in list	*
C*	IFCSTM		INTEGER		Number of forecast times        *
C*	IRHOUR		INTEGER		Report hour                     *
C*									*
C* Output parameters:							*
C*	RDATA		REAL		Forecast data			*
C*	  (IFCSTM,MMPARM)						*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	9/00                                            *
C* D. Kidwell/NCEP     11/01	Fixed X/N, N/X and T12 for 06 and 18Z   *
C* m.gamazaychikov/SAIC 11/03   Replaced references to AVN/MRF with     *
C*                              references to GFS/GFSX                  *
C* D. Kidwell/NCEP	3/04 	Add 'FW' as a CLD value                 *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	report
	INTEGER		istart (*), iprms (*)
	REAL		rdata (ifcstm,MMPARM)
C*	
	CHARACTER	cline (50)*3, tstr*4
	INTEGER		line (50), lineb (50)
	PARAMETER	( INCR = 3 )
C------------------------------------------------------------------------
	iret = 0
C
C*	Decode each line of the GFS MOS report.
C
C*	X/N -> TDYF, TNTF and N/X -> TNTF, TDYF
C
	istrt = istart ( 1 ) 
	IF ( istrt .ne. 0 ) THEN
	    IF ( ( irhour .eq. 0 ) .or. ( irhour .eq. 12 ) ) THEN
		istrt  = istrt + 18
                initft = 7
	      ELSE
		istrt  = istrt + 12
 		initft = 5
	    END IF
	END IF
	CALL MS_MXMN ( report, istrt, 4*INCR, ifcstm, initft, line,
     +		       lineb, ier )
	IF ( ( irhour .eq. 0 ) .or. ( irhour .eq. 6 ) ) THEN
	    iprm1 = iprms ( 2 )
	    iprm2 = iprms ( 1 )
	  ELSE 
	    iprm1 = iprms ( 1 )
	    iprm2 = iprms ( 2 )
	END IF
	CALL MS_MISS ( 999, line, iprm1, ifcstm, rdata, ier )
	CALL MS_MISS ( 999, lineb, iprm2, ifcstm, rdata, ier )
C
C*	TMP -> TMPF
C
	CALL MS_IDAT ( report, istart (2), INCR, ifcstm, line, ier )
	CALL MS_MISS ( 999, line, iprms (3), ifcstm, rdata, ier )
C
C*	DPT -> DWPF
C
	CALL MS_IDAT ( report, istart (3), INCR, ifcstm, line, ier )
	CALL MS_MISS ( 999, line, iprms (4), ifcstm, rdata, ier )
C
C*	CLD -> CLCT
C
	CALL MS_CDAT ( report, istart (4), INCR, ifcstm, cline, ier )
	DO ii = 1, ifcstm
	    IF ( cline ( ii ) .eq. ' CL' ) THEN
		tstr = 'CLR'
	      ELSE IF ( cline ( ii ) .eq. ' FW' ) THEN
		tstr = '-SCT'
	      ELSE IF ( cline ( ii ) .eq. ' SC' ) THEN
		tstr = 'SCT'
	      ELSE IF ( cline ( ii ) .eq. ' BK' ) THEN
		tstr = 'BKN'
	      ELSE IF ( cline ( ii ) .eq. ' OV' ) THEN
		tstr = 'OVC'
	      ELSE
		tstr = ' '
	    END IF
	    rdata ( ii, iprms ( 5 ) ) = PT_CCNM ( tstr )
	END DO
C
C*	WDR -> DRCT
C
	CALL MS_IDAT ( report, istart (5), INCR, ifcstm, line, ier )
	DO ii = 1, ifcstm
	    IF ( line ( ii ) .ne. IMISSD ) 
     +		 line ( ii ) = line ( ii ) * 10
	END DO
	CALL MS_MISS ( 990, line, iprms (6), ifcstm, rdata, ier )
C
C*	WSP -> SKNT
C
	CALL MS_IDAT ( report, istart (6), INCR, ifcstm, line, ier )
	CALL MS_MISS ( 99, line, iprms (7), ifcstm, rdata, ier )
C
C*	P06 -> PP06
C
	CALL MS_IDAT ( report, istart (7), INCR, ifcstm, line, ier )
	CALL MS_MISS ( 999, line, iprms (8), ifcstm, rdata, ier )
C
C*	P12 -> PP12
C
	CALL MS_IDAT ( report, istart (8), INCR, ifcstm, line, ier )
	CALL MS_MISS ( 999, line, iprms (9), ifcstm, rdata, ier )
C
C*	T06 -> TS06, TC06
C
	istrt = istart ( 9 )
	IF ( istrt .ne. 0 ) istrt = istrt + 3
	CALL MS_IDT2 ( report, istrt, 2*INCR, ifcstm, 3, line, lineb,
     +		       ier )
	CALL MS_MISS ( 999, line, iprms (12), ifcstm, rdata, ier )
	CALL MS_MISS ( 99, lineb, iprms (13), ifcstm, rdata, ier )
C
C*	T12 -> TS12, TC12
C
	istrt = istart ( 10 )
	IF ( istrt .ne. 0 ) THEN
	    IF ( ( irhour .eq. 0 ) .or. ( irhour .eq. 12 ) ) THEN
		istrt  = istrt + 9
		initft = 5
	      ELSE
		istrt  = istrt + 15
		initft = 7
	    END IF
	END IF
	CALL MS_IDT2 ( report, istrt, 4*INCR, ifcstm, initft, line,
     +		       lineb, ier )
	CALL MS_MISS ( 999, line, iprms (14), ifcstm, rdata, ier )
	CALL MS_MISS ( 99, lineb, iprms (15), ifcstm, rdata, ier )
C
C*	POZ -> POZP
C
	CALL MS_IDAT ( report, istart (11), INCR, ifcstm, line, ier )
	CALL MS_MISS ( 999, line, iprms (17), ifcstm, rdata, ier )
C
C*	POS -> POSN
C
	CALL MS_IDAT ( report, istart (12), INCR, ifcstm, line, ier )
	CALL MS_MISS ( 999, line, iprms (18), ifcstm, rdata, ier )
C
C*	TYP -> PCPT
C
	CALL MS_CDAT ( report, istart (13), INCR, ifcstm, cline, ier )
	CALL MS_PTYP ( cline, iprms (16), ifcstm, rdata, ier )
C
C*	Q06 -> QP06
C
	CALL MS_IDAT ( report, istart (14), INCR, ifcstm, line, ier )
	CALL MS_MISS ( 9, line, iprms (10), ifcstm, rdata, ier )
C
C*	Q12 -> QP12
C
	CALL MS_IDAT ( report, istart (15), INCR, ifcstm, line, ier )
	CALL MS_MISS ( 9, line, iprms (11), ifcstm, rdata, ier )
C
C*	SNW -> SN24
C
	CALL MS_IDAT ( report, istart (16), INCR, ifcstm, line, ier )
	CALL MS_MISS ( 9, line, iprms (19), ifcstm, rdata, ier )
C
C*	CIG -> FCIG
C
	CALL MS_IDAT ( report, istart (17), INCR, ifcstm, line, ier )
	CALL MS_MISS ( 9, line, iprms (20), ifcstm, rdata, ier )
C
C*	VIS -> FVSA
C
	CALL MS_IDAT ( report, istart (18), INCR, ifcstm, line, ier )
	CALL MS_MISS ( 9, line, iprms (21), ifcstm, rdata, ier )
C
C*	OBV -> OVIS
C
	CALL MS_CDAT ( report, istart (19), INCR, ifcstm, cline, ier )
	iparm = iprms ( 22)
	DO ii = 1, ifcstm
	    IF ( cline ( ii ) .eq. '  N' ) THEN
		rdata ( ii, iparm ) = 0.
	      ELSE IF ( cline ( ii ) .eq. ' FG' ) THEN
		rdata ( ii, iparm ) = 1.
	      ELSE IF ( cline ( ii ) .eq. ' HZ' ) THEN
		rdata ( ii, iparm ) = 2.
	      ELSE IF ( cline ( ii ) .eq. ' BR' ) THEN
		rdata ( ii, iparm ) = 3.
	      ELSE IF ( cline ( ii ) .eq. ' BL' ) THEN
		rdata ( ii, iparm ) = 4.
	      ELSE
		rdata ( ii, iparm ) = RMISSD
	    END IF
	END DO
C*
	RETURN
	END
