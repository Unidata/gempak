	SUBROUTINE MS_DCDM ( report, istart, iprms, ifcstm, irhour,
     +			     rdata, iret ) 
C************************************************************************
C* MS_DCDM								*
C*									*
C* This subroutine decodes the data contained in a GFSX MOS report.	*
C*									*
C* MS_DCDM ( REPORT, ISTART, IPRMS, IFCSTM, IRHOUR, RDATA, IRET )	*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		GFSX MOS report			*
C*	ISTART (*)	INTEGER		Indices of line beginnings	*
C*	IPRMS (*)	INTEGER		Position of parameter in list	*
C*	IFCSTM		INTEGER		Number of forecast times        *
C*	IRHOUR		INTEGER		Report hour			*
C*									*
C* Output parameters:							*
C*	RDATA		REAL		Forecast data			*
C*	  (IFCSTM,MMPARM)						*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	9/00                                            *
C* D. Kidwell/NCEP     10/00 	PRN --> PRS                             *
C* F. J. Yen/NCEP	8/01	Replaced WND with WSP and added WDR	*
C* D. Kidwell/NCEP	9/02	Bug fixes for climo; WSPD as WND/WSP    *
C* m.gamazaychikov/SAIC 11/03   Replaced references to AVN/MRF with     *
C*                              references to GFS/GFSX                  *
C* m.gamazaychikov/SAIC 07/05	Changed to allow decoding of 12Z data	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	report
	INTEGER		istart (*), iprms (*)
	REAL		rdata (ifcstm,MMPARM)
C*	
	CHARACTER	cline (50)*3, tstr*4, dirchr*4
	INTEGER		line (50), lineb (50)
	PARAMETER	( INCR = 4 )
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret   = 0
	iclimo = ifcstm + 2
C
C*	Decode each line of the GFSX MOS report.
C
C*	X/N -> TDYF, TNTF
C
	CALL MS_MXMN ( report, istart (1), INCR, iclimo, 1, line, 
     +		       lineb, ier )
        IF ( irhour .eq. 0 ) THEN
	   iprm1 = iprms (2)
	   iprm2 = iprms (1)
	 ELSE
	   iprm1 = iprms (1)
	   iprm2 = iprms (2)
	END IF
	CALL MS_MISS ( 999, line,  iprm1, ifcstm, rdata, ier )
        CALL MS_MISS ( 999, lineb, iprm2, ifcstm, rdata, ier )
C
C*	Store the day 5 climatic normals for min/max temperature.
C*	TNCF, TDCF
C
	itncf = lineb ( iclimo - 1 )
	itdcf = line ( iclimo )
        IF ( irhour .eq. 0 ) THEN
	   DO ii = 1, ifcstm, 2
	      line  ( ii )     = itdcf
	      line  ( ii + 1 ) = IMISSD
	      lineb ( ii )     = IMISSD
	      lineb ( ii + 1 ) = itncf
	   END DO
	 ELSE
	   DO ii = 1, ifcstm, 2
	      line  ( ii )     = IMISSD
	      line  ( ii + 1 ) = itdcf
	      lineb ( ii )     = itncf
	      lineb ( ii + 1 ) = IMISSD
	   END DO
	END IF
	CALL MS_MISS ( 999, line, iprms ( 19 ), ifcstm, rdata, ier )
	CALL MS_MISS ( 999, lineb, iprms ( 18 ), ifcstm, rdata, ier )
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
C*	CLD -> CL12
C
	CALL MS_CDAT ( report, istart (4), INCR, ifcstm, cline, ier )
	DO ii = 1, ifcstm
	    IF ( cline ( ii ) .eq. ' CL' ) THEN
		tstr = 'CLR'
	      ELSE IF ( cline ( ii ) .eq. ' PC' ) THEN
		tstr = '-BKN'
	      ELSE IF ( cline ( ii ) .eq. ' OV' ) THEN
		tstr = 'OVC'
	      ELSE
		tstr = ' '
	    END IF
	    rdata ( ii, iprms ( 5 ) ) = PT_CCNM ( tstr )
	END DO
C
C*	WSP or WND -> SK12
C
	CALL MS_IDAT ( report, istart (5), INCR, ifcstm, line, ier )
	CALL MS_MISS ( 999, line, iprms (6), ifcstm, rdata, ier )
C
C*	P12 -> PP12
C
	CALL MS_IDAT ( report, istart (6), INCR, -iclimo, line, ier )
	CALL MS_MISS ( 999, line, iprms (7), ifcstm, rdata, ier )
C
C*	Store the day 5 climatic normals for 12-hr precip probability.
C*	PP1C
C
	ip012c = line ( iclimo - 1 )
	ip120c = line ( iclimo )
        IF ( irhour .eq. 0 ) THEN
	   DO ii = 1, ifcstm, 2
	      line ( ii )     = ip120c
	      line ( ii + 1 ) = ip012c
	   END DO
	 ELSE
	   DO ii = 1, ifcstm, 2
	      line ( ii )     = ip012c
	      line ( ii + 1 ) = ip120c
	   END DO
	END IF
	CALL MS_MISS ( 999, line, iprms ( 20 ), ifcstm, rdata, ier )
C
C*	P24 -> PP24
C
	CALL MS_IDAT ( report, istart (7), INCR, -iclimo, line, ier )
	CALL MS_MISS ( 999, line, iprms (8), ifcstm, rdata, ier )
C
C*	Store the day 5 climatic normal for 24-hr precip probability.
C*	PP2C
C
	ipp24c = line ( iclimo )
        IF ( irhour .eq. 0 ) THEN
	   DO ii = 1, ifcstm, 2
	      line ( ii )     = ipp24c
	      line ( ii + 1 ) = IMISSD
	   END DO
	 ELSE
	   DO ii = 1, ifcstm, 2
	      line ( ii )     = IMISSD
	      line ( ii + 1 ) = ipp24c
	   END DO
	END IF
	CALL MS_MISS ( 999, line, iprms ( 21 ) , ifcstm, rdata, ier )
C
C*	Q12 -> QP12
C
	CALL MS_IDAT ( report, istart (8), INCR, ifcstm, line, ier )
	CALL MS_MISS ( 9, line, iprms (9), ifcstm, rdata, ier )
C
C*	Q24 -> QP24
C
	CALL MS_IDAT ( report, istart (9), INCR, ifcstm, line, ier )
	CALL MS_MISS ( 9, line, iprms (10), ifcstm, rdata, ier )
C
C*	T12 -> TS12
C
	CALL MS_IDAT ( report, istart (10), INCR, ifcstm, line, ier )
	CALL MS_MISS ( 999, line, iprms (11), ifcstm, rdata, ier )
C
C*	T24 -> TS24
C
	CALL MS_IDAT ( report, istart (11), INCR, ifcstm, line, ier )
	CALL MS_MISS ( 999, line, iprms (12), ifcstm, rdata, ier )
C
C*	PZP -> PZ12
C
	CALL MS_IDAT ( report, istart (12), INCR, ifcstm, line, ier )
	CALL MS_MISS ( 999, line, iprms (14), ifcstm, rdata, ier )
C
C*	PSN -> PS12
C
	CALL MS_IDAT ( report, istart (13), INCR, ifcstm, line, ier )
	CALL MS_MISS ( 999, line, iprms (15), ifcstm, rdata, ier )
C
C*	PRS -> PR12
C
	CALL MS_IDAT ( report, istart (14), INCR, ifcstm, line, ier )
	CALL MS_MISS ( 999, line, iprms (16), ifcstm, rdata, ier )
C
C*	TYP -> PC12
C
	CALL MS_CDAT ( report, istart (15), INCR, ifcstm, cline, ier )
	CALL MS_PTYP ( cline, iprms (13), ifcstm, rdata, ier )
C
C*	SNW -> SN24
C
	CALL MS_IDAT ( report, istart (16), INCR, ifcstm, line, ier )
	CALL MS_MISS ( 9, line, iprms (17), ifcstm, rdata, ier )
C
C*	WDR -> DRCT
C
	CALL MS_CDAT ( report, istart (17), INCR, ifcstm, cline, ier )
        DO ii = 1, ifcstm
            CALL ST_RMBL ( cline (ii), dirchr, len, ier )
            CALL ST_NULL ( dirchr, dirchr, len, ier )
            CALL CLO_CMPDIR ( dirchr, wdir, ier )
            rdata ( ii, iprms(26) ) = wdir
        END DO
C
C*	Anomalies - TNAF, TDAF, PP1A, PP2A
C
	DO jj = 22, 25
	    jclim = jj - 4
	    jdata = jj - 21
	    IF ( jdata .gt. 2 ) jdata = jdata + 4
	    DO ii = 1, ifcstm
		IF ( ( ERMISS ( rdata ( ii, iprms ( jclim ) ) ) ) .or.
     +		     ( ERMISS ( rdata ( ii, iprms ( jdata ) ) ) ) ) THEN
		    rdata ( ii, iprms ( jj ) ) = RMISSD
		  ELSE
		    rdata ( ii, iprms ( jj ) ) = 
     +					rdata ( ii, iprms ( jdata ) ) - 
     +					rdata ( ii, iprms ( jclim ) )
		END IF
	    END DO
	END DO
C*
	RETURN
	END
