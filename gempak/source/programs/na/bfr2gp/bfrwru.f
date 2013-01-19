	SUBROUTINE BFRWRU ( lunsn, ihhmm, np, nz, sndata, iret )
C************************************************************************
C* BFRWRU								*
C*									*
C* This subroutine adds data to an unmerged sounding file.		* 
C*									*
C* The following parameters must be stored in sndata at nz levels:	*
C*									*
C* 		PRES HGHT TEMP DWPT DRCT SPED LFLG			*
C*									*
C* An unmerged sounding data file will be written.  LFLG will be checked*
C* to make the following correspondences:				*
C*									*
C*	INTEGER VALUE		DATA TYPE				*
C*									*
C*	    64			Surface					*
C*	    16			Tropopause level			*
C*	     8			Maximum wind level			*
C*	    32			Mandatory level				*
C*	     4			Significant temperature level		*
C*	     2			Significant wind level			*
C*									*
C*									*
C* BFRWRU ( LUNSN, IHHMM, NP, NZ, SNDATA, IRET )			*
C*									*
C* Input parameters:							*
C*	LUNSN		INTEGER		Unit number of unmerged snd file*
C*	IHHMM		INTEGER		Hour minuute obs time		*
C*	NP		INTGER		Number of parameters		*
C*	NZ		INTEGER		Number of levels		*
C*	SNDATA (NP,NZ)	REAL		Sounding data			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C*					-31 = cannot write man data	*
C*					-32 = cannot write sig data	*
C*					-35 = wrong # of parameters	*
C*					-36 = sig wind on both p and z	*
C**									*
C* Log:									*
C* K. Brill/EMC		 2/97						*
C* K. Brill/EMC		 7/98	Chk # of levels before writing parts	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		sndata (np,nz)
C*
	REAL		snbuf (LLMXLV*6)
	CHARACTER*4	part
	LOGICAL		windz, pflag
C*
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
	iret = 0
	IF ( np .ne. 7 ) THEN
	    iret = -35
	    RETURN
	END IF
C*
	sprs = RMISSD
	shgt = RMISSD
	stmp = RMISSD
	sdwp = RMISSD
	sddd = RMISSD
	ssss = RMISSD
	sprs = RMISSD
C*
	tprs = RMISSD
	thgt = RMISSD
	ttmp = RMISSD
	tdwp = RMISSD
	tddd = RMISSD
	tsss = RMISSD
C*
	wprs = RMISSD
	whgt = RMISSD
	wtmp = RMISSD
	wdwp = RMISSD
	wddd = RMISSD
	wsss = RMISSD
C
C*	Find single level data at surface, max wind, and trop.
C
	DO iz = 1, nz
	    prs = sndata (1,iz)
	    hgt = sndata (2,iz)
	    tmp = sndata (3,iz)
	    dwp = sndata (4,iz)
	    ddd = sndata (5,iz)
	    sss = sndata (6,iz)
	    lfg = NINT ( sndata (7,iz) )
	    IF ( lfg .eq. 64 ) THEN
		sprs = prs
		shgt = hgt
		stmp = tmp
		sdwp = dwp
		sddd = ddd
		ssss = sss
	    ELSE IF ( lfg .eq. 16 ) THEN
		tprs = prs
		thgt = hgt
		ttmp = tmp
		tdwp = dwp
		tddd = ddd
		tsss = sss
	    ELSE IF ( lfg .eq. 8 ) THEN
		wprs = prs
		whgt = hgt
		wtmp = tmp
		wdwp = dwp
		wddd = ddd
		wsss = sss
	    END IF
	END DO
C
C*	Now process mandatory level data.
C
	iman = 0
	indx = 0
	nzabv = 0
	nzblo = 0
	DO iz = 1, nz
	    prs = sndata (1,iz)
	    hgt = sndata (2,iz)
	    tmp = sndata (3,iz)
	    dwp = sndata (4,iz)
	    ddd = sndata (5,iz)
	    sss = sndata (6,iz)
	    lfg = NINT ( sndata (7,iz) )
	    IF ( lfg .eq. 32 ) THEN	
		IF ( .not. ERMISS ( prs ) ) THEN
		    iman = iman + 1
		    indx = indx + 1
		    snbuf (indx) = prs
		    indx = indx + 1
		    snbuf (indx) = tmp
		    indx = indx + 1
		    snbuf (indx) = dwp
		    indx = indx + 1
		    snbuf (indx) = ddd
		    indx = indx + 1
		    snbuf (indx) = sss
		    indx = indx + 1
		    snbuf (indx) = hgt
		END IF
	    END IF
	END DO
	CALL BFRSRT ( .true., 6, iman, snbuf, lv1abv, ier )
	IF ( lv1abv .ne. 0 ) THEN
	    nzabv = iman - lv1abv + 1
	    nzblo = iman - nzabv
	    inabv = nzblo * 6 + 1
	ELSE
	    nzblo = iman
	END IF
	IF ( lv1abv .ne. 1 .and. nzblo .gt. 0 ) THEN
	    part = 'TTAA'
	    CALL SN_WPRT ( lunsn, part, ihhmm, nzblo, snbuf,
     +			   .false., ier )
	    IF ( ier .ne. 0 ) THEN
		iret = -31
		RETURN
	    END IF
	END IF
	IF ( lv1abv .ne. 0 .and. nzabv .gt. 0 ) THEN
	    part = 'TTCC'
	    CALL SN_WPRT ( lunsn, part, ihhmm, nzabv, snbuf (inabv),
     +			   .false., ier )
	    IF ( ier .ne. 0 ) THEN
		iret = -31
		RETURN
	    END IF
	END IF
C
C*	Treat single level thermal data as significant level data.
C
	isig = 0
	indx = 0
	nzabv = 0
	nzblo = 0
	IF ( .not. ERMISS (sprs) .and. .not. ERMISS (stmp) ) THEN
	    isig = isig + 1
	    snbuf (1) = sprs
	    snbuf (2) = stmp
	    snbuf (3) = sdwp
	    indx = 3
	END IF
	IF ( .not. ERMISS (tprs) .and. .not. ERMISS (ttmp) ) THEN
	    isig = isig + 1
	    indx = indx + 1
	    snbuf ( indx ) = tprs
	    indx = indx + 1
	    snbuf ( indx ) = ttmp
	    indx = indx + 1
	    snbuf ( indx ) = tdwp
	END IF
	IF ( .not. ERMISS (wprs) .and. .not. ERMISS (wtmp) ) THEN
	    isig = isig + 1
	    indx = indx + 1
	    snbuf ( indx ) = wprs
	    indx = indx + 1
	    snbuf ( indx ) = wtmp
	    indx = indx + 1
	    snbuf ( indx ) = wdwp
	END IF
C
C*	Look for significant temperature data.
C
	DO iz = 1, nz
	    prs = sndata (1,iz)
	    tmp = sndata (3,iz)
	    dwp = sndata (4,iz)
	    lfg = NINT ( sndata (7,iz) )
	    IF ( lfg .eq. 4 ) THEN	
		IF ( .not. ERMISS ( prs ) ) THEN
		    isig = isig + 1
		    indx = indx + 1
		    snbuf (indx) = prs
		    indx = indx + 1
		    snbuf (indx) = tmp
		    indx = indx + 1
		    snbuf (indx) = dwp
		END IF
	    END IF
	END DO
	CALL BFRSRT ( .true., 3, isig, snbuf, lv1abv, ier )
	IF ( lv1abv .ne. 0 ) THEN
	    nzabv = isig - lv1abv + 1
	    nzblo = isig - nzabv
	    inabv = nzblo * 3 + 1
	ELSE
	    nzblo = isig
	END IF
	IF ( lv1abv .ne. 1 .and. nzblo .gt. 0 ) THEN
	    part = 'TTBB'
	    CALL SN_WPRT ( lunsn, part, ihhmm, nzblo, snbuf,
     +			   .false., ier )
	    IF ( ier .ne. 0 ) THEN
		iret = -31
		RETURN
	    END IF
	END IF
	IF ( lv1abv .ne. 0 .and. nzabv .gt. 0 ) THEN
	    part = 'TTDD'
	    CALL SN_WPRT ( lunsn, part, ihhmm, nzabv, snbuf (inabv),
     +			   .false., ier )
	    IF ( ier .ne. 0 ) THEN
		iret = -31
		RETURN
	    END IF
	END IF
C
C*	Look for significant wind data.
C
	isig = 0
	indx = 0
	nzabv = 0
	nzblo = 0
	pflag = .true.
	DO iz = 1, nz
	    prs = sndata (1,iz)
	    hgt = sndata (2,iz)
	    ddd = sndata (5,iz)
	    sss = sndata (6,iz)
	    lfg = NINT ( sndata (7,iz) )
	    IF ( lfg .eq. 2 ) THEN	
		IF ( .not. ERMISS ( prs ) ) THEN
		    IF ( .not. pflag ) THEN
			iret = -36
			RETURN
		    END IF
		    isig = isig + 1
		    indx = indx + 1
		    snbuf (indx) = prs
		    indx = indx + 1
		    snbuf (indx) = ddd
		    indx = indx + 1
		    snbuf (indx) = sss
		ELSE IF ( .not. ERMISS ( hgt ) ) THEN
		    IF ( pflag .and. isig .gt. 0 ) THEN
			iret = -36
			RETURN
		    END IF
		    pflag = .false.
		    isig = isig + 1
		    indx = indx + 1
		    snbuf (indx) = hgt
		    indx = indx + 1
		    snbuf (indx) = ddd
		    indx = indx + 1
		    snbuf (indx) = sss
		END IF
	    END IF
	END DO
C
C*	Treat single level wind data as significant level data.
C
	IF ( .not. ERMISS (sprs) .and. .not. ERMISS (sddd) .and.
     +	     pflag) THEN
	    isig = isig + 1
	    indx = indx + 1
	    snbuf (indx) = sprs
	    indx = indx + 1
	    snbuf (indx) = sddd
	    indx = indx + 1
	    snbuf (indx) = ssss
	END IF
	IF ( .not. ERMISS (tprs) .and. .not. ERMISS (tddd) .and.
     +       pflag ) THEN
	    isig = isig + 1
	    indx = indx + 1
	    snbuf ( indx ) = tprs
	    indx = indx + 1
	    snbuf ( indx ) = tddd
	    indx = indx + 1
	    snbuf ( indx ) = tsss
	END IF
	IF ( .not. ERMISS (wprs) .and. .not. ERMISS (wddd) .and.
     +       pflag ) THEN
	    isig = isig + 1
	    indx = indx + 1
	    snbuf ( indx ) = wprs
	    indx = indx + 1
	    snbuf ( indx ) = wddd
	    indx = indx + 1
	    snbuf ( indx ) = wsss
	END IF
	IF ( .not. ERMISS (shgt) .and. .not. ERMISS (sddd) .and.
     +	     .not. pflag) THEN
	    isig = isig + 1
	    indx = indx + 1
	    snbuf (indx) = shgt
	    indx = indx + 1
	    snbuf (indx) = sddd
	    indx = indx + 1
	    snbuf (indx) = ssss
	END IF
	IF ( .not. ERMISS (thgt) .and. .not. ERMISS (tddd) .and.
     +       .not. pflag ) THEN
	    isig = isig + 1
	    indx = indx + 1
	    snbuf ( indx ) = thgt
	    indx = indx + 1
	    snbuf ( indx ) = tddd
	    indx = indx + 1
	    snbuf ( indx ) = tsss
	END IF
	IF ( .not. ERMISS (whgt) .and. .not. ERMISS (wddd) .and.
     +       .not. pflag ) THEN
	    isig = isig + 1
	    indx = indx + 1
	    snbuf ( indx ) = whgt
	    indx = indx + 1
	    snbuf ( indx ) = wddd
	    indx = indx + 1
	    snbuf ( indx ) = wsss
	END IF
	CALL BFRSRT ( pflag, 3, isig, snbuf, lv1abv, ier )
	windz = .not. pflag
	IF ( lv1abv .ne. 0 ) THEN
	    nzabv = isig - lv1abv + 1
	    nzblo = isig - nzabv
	    inabv = nzblo * 3 + 1
	ELSE
	    nzblo = isig
	END IF
	IF ( lv1abv .ne. 1 .and. nzblo .gt. 0 ) THEN
	    part = 'PPBB'
	    CALL SN_WPRT ( lunsn, part, ihhmm, nzblo, snbuf,
     +			   windz, ier )
	    IF ( ier .ne. 0 ) THEN
		iret = -31
		RETURN
	    END IF
	END IF
	IF ( lv1abv .ne. 0 .and. nzabv .gt. 0 ) THEN
	    part = 'PPDD'
	    CALL SN_WPRT ( lunsn, part, ihhmm, nzabv, snbuf (inabv),
     +			   windz, ier )
	    IF ( ier .ne. 0 ) THEN
		iret = -31
		RETURN
	    END IF
	END IF
C*
	RETURN
	END
