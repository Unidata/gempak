        SUBROUTINE NAMJEN ( lunjen, nprmsn, nprmsf, parmsn, parmsf,
     +			    dattim, stid, istnm, slat, slon, selv,
     +			    sndata, sfdata, nz, ifhr, iret )
C************************************************************************
C* NAMJEN								*
C*									*
C* This subroutine write data to an ASCII output file to accomodate	*
C* PC applications.							*
C*									*
C* NAMJEN  ( LUNJEN, NPRMSN, NPRMSF, PARMSN, PARMSF, DATTIM, STID,	*
C*	     ISTNM, SLAT, SLON, SELV, SNDATA, SFDATA, NZ, IFHR, IRET )	*
C*									*
C* Input parameters:							*
C*	LUNJEN		INTEGER		Unit number of ASCII output file*
C*	NPRMSN 		INTEGER		Number of profile parms		*
C*	NPRMSF		INTEGER		Number of surface parms		*
C*	PARMSN(*)	CHAR*4		Sounding parameter list		*
C*	PARMSF(*)	CHAR*4		Surface parameter list		*
C*	DATTIM		CHAR*		Date/time			*
C*	STID		CHAR*		Station ID			*
C*	ISTNM		INTEGER		Station number			*
C*	SLAT		REAL		Station latitude		*
C*	SLON		REAL		Station longitude		*
C*	SELV		REAL		Station elevation		*
C*	SNDATA (*)	REAL		Array of sounding data		*
C*	SFDATA (*)	REAL		Array of surface data		*
C*	NZ		INTEGER		Number of levels in sounding	*
C*	IFHR		INTEGER		Forecast hour of profile	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C**									*
C* Log:									*
C* K. Brill/NMC		 8/95   					*
C* K. Brill/EMC		 9/96	Check for 6-digit ID			*
C* K. Brill/EMC		12/98	Check for missing precip		*
C* D. Kidwell/NCEP	12/98	SNMJEN -> NAMJEN                        *
C* T. Lee/GSC		12/98	Fixed FORMAT statement, X -> 1X		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	parmsn (*), parmsf (*), stid, dattim
	REAL		sndata (*), sfdata (*)
C*
	CHARACTER*1	clat, clon
	CHARACTER*8	ymdh
C*
	SAVE		istn1, ttlprc, ymdh
C*
	INCLUDE		'ERMISS.FNC'
C----------------------------------------------------------------------
	iret = 0
C
C*	Write station header.
C
	IF ( ifhr .eq. 0 ) THEN
	    ymdh = dattim (1:6)
	    ymdh = ymdh (1:6) // dattim (8:9)
	    ttlprc = 0.
	    istn1 = istnm
	    IF ( slat .ge. 0 ) THEN
		clat = 'N'
	    ELSE
		clat = 'S'
	    ENDIF
	    IF ( slon .ge. 0 ) THEN
		clon = 'E'
	    ELSE
		clon = 'W'
	    END IF
	    rlat = ABS ( slat )
	    rlon = ABS ( slon )
	    WRITE ( lunjen, 1001 ) rlat, clat, rlon, clon, stid
1001	    FORMAT ( 'STATION = ', F6.2, A1, 1X, F6.2, A1, 1X, A )
	    IF ( istnm .le. 99999 ) THEN
	        WRITE ( lunjen, 1002 ) ymdh, istnm
1002	        FORMAT ( A8, 1X, I5 )
	    ELSE
	        WRITE ( lunjen, 1022 ) ymdh, istnm
1022	        FORMAT ( A8, 1X, I6 )
	    END IF
	    WRITE ( lunjen, 1003 )
1003	    FORMAT ( 52('*') )
	ELSE IF ( istn1 .ne. istnm ) THEN
	    RETURN
	END IF
C
C*	Write output header.
C
	WRITE ( lunjen, 1004 ) stid(1:3), ymdh, ifhr
1004	FORMAT ( 1X, A3, 2X, A8, ' FORECAST PROJECTION=', I2 )
	WRITE ( lunjen, 1005 )
1005	FORMAT ( ' LYR  TEMP DEPR   KTS DIR  PRES   RH  OMEG' )
	ind = nprmsn * ( nz - 1 )
C
C*	Loop over sounding levels starting at the top.
C
	DO k = nz, 1, -1
C
C*	    Now loop over the output parameters.
C
	    DO ip = 1, nprmsn
		io = ip + ind
		IF ( parmsn (ip) .eq. 'PRES' ) THEN
		    pres = sndata (io)
		ELSE IF ( parmsn (ip) .eq. 'TMPC' ) THEN
		    tmpc = sndata (io)
		ELSE IF ( parmsn (ip) .eq. 'DWPC' ) THEN
		    dwpt = sndata (io)
		ELSE IF ( parmsn (ip) .eq. 'SPED' ) THEN
		    spd = sndata (io) * 1.9438
		ELSE IF ( parmsn (ip) .eq. 'DRCT' ) THEN
		    dir = sndata (io)
		ELSE IF ( parmsn (ip) .eq. 'OMEG' ) THEN
		    omeg = sndata (io) * 10.
		END IF
	    END DO
	    ind = ind - nprmsn
	    relh = PR_RELH ( tmpc, dwpt )
	    IF ( ERMISS ( relh ) ) relh = 0.
	    dwpd = tmpc - dwpt
	    IF ( dwpd .gt. 99. ) dwpd = 99.
	    WRITE ( lunjen, 1006 ) k, tmpc, dwpd, spd, dir,
     +				   pres, relh, omeg
1006	    FORMAT ( 1X, I3, 1X, F5.1, 1X, F4.1, 1X, F5.1, 1X,
     +               F4.0, 1X, F5.0, 1X, F4.0, 1X, F5.0 )
	END DO
C
C*	Get the surface pressure and the precip.
C
	pres = 0.
	prcp = 0.
	DO i = 1, nprmsf
	    IF ( parmsf (i) .eq. 'PRES' ) pres = sfdata (i)
	    IF ( parmsf (i) .eq. 'P01M' ) THEN
		IF ( .not. ERMISS ( sfdata (i) ) ) THEN
		    prcp = sfdata (i) / 25.4
		ELSE
		    prcp = 0.0
		END IF
	    END IF
	END DO
	ttlprc = ttlprc + prcp
	iprcp = NINT ( prcp * 1000. )
	WRITE ( lunjen, 1007 ) iprcp, ttlprc, pres
1007	FORMAT ( ' PRCP(HR)=.', I3.3, '   PRCP(TOT)=', F6.3,
     +           '   SFC PRES=', F5.0 )
	WRITE ( lunjen, 1003 )
C*
	RETURN
	END
