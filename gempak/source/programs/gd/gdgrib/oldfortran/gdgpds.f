	SUBROUTINE GDGPDS ( pdsval, vercen, rnvblk, gparm,
     +			    ivcord, level, havbms, ip10sc, lasttm,
     +			    gdttm, gbtbls, igpds, nbpds,
     +			    cpds, cdd, chhmm, iret )
C************************************************************************
C* GDGPDS								*
C*									*
C* This subroutine bridges the gap between the user input and PDS_MAKE.	*
C* The PDS byte array is returned.					*
C* 									*
C* GDGPDS  ( PDSVAL, VERCEN, RNVBLK, GPARM, IVCORD, LEVEL, HAVBMS,	*
C*           IP10SC, LASTTM, GDTTM, GBTBLS, IGPDS, NBPDS,
C*	     CPDS, CDD, CHHMM, IRET )					*
C*									*
C* Input parameters:							*
C*	PDSVAL		CHAR*		User input override PDS numbers	*
C*	VERCEN		CHAR*		User input for octets 4,5,6,26  *
C*	RNVBLK (LLNNAV) REAL		Grid navigation block		*
C*	GPARM		CHAR*		Grid parameter from DG_GRID	*
C*	IVCORD		INTEGER		Grid vert coord # from DG_GRID	*
C*	LEVEL (2)	INTEGER		Grid level from DG_GRID		*
C*	HAVBMS		LOGICAL		Flag for existence of BMS	*
C*	IP10SC		INTEGER		Power of 10 scale factor	*
C*	LASTTM		CHAR*		Last grid time			*
C*	GDTTM (2)	CHAR*		Grid time from DG_GRID		*
C*	GBTBLS		CHAR*		User input for GRIB tables	*
C*	IGPDS		INTEGER		Grid navigation # from CPYFIL	*
C*									*
C* Input and output parameter:						*
C*	NBPDS		INTEGER		Input:  Max length for PDS	*
C*					Output: Actual length of PDS	*
C* Output parameters:							*
C*	CPDS (NBPDS)	CHAR*1		PDS array			*
C*	CDD		CHAR*2		2-digit day of month		*
C*	CHHMM		CHAR*4		Hour minute of data		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. Brill/HPC		08/99						*
C* K. Brill/HPC		 2/00	Add IGPDS				*
C* K. Brill/HPC		 3/00	Avoid character assignment to itself	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	CHARACTER*(*)	pdsval, vercen, gparm, gdttm (2), gbtbls
	CHARACTER*(*)	lasttm
	CHARACTER*1	cpds (*)
	CHARACTER	cdd*2, chhmm*4
	REAL		rnvblk (*)
	INTEGER		level (2)
	LOGICAL		havbms
C*
	CHARACTER*4	gvcord
	CHARACTER*72	gdnbuf*128
	CHARACTER*20	flgdtm (2)
	CHARACTER	oparm*12, ovcrd*12, olevl*16, c2d*2, cdum*2
	INTEGER		ipos (4)
C------------------------------------------------------------------------
C------------------------------------------------------------------------
	iret = 0
C
C*	Decode the override grid identifier parameters:
C
	CALL ST_LCUC ( pdsval, gdnbuf, ier )
C*
	CALL ST_LSTR ( gdnbuf, len, ier )
	ihat = INDEX ( gdnbuf, '^' )
	iaat = INDEX ( gdnbuf, '@' )
	ipct = INDEX ( gdnbuf, '%' )
C
C*	Locate endings of substrings.
C
	ipos (1) = ihat
	ipos (2) = iaat
	ipos (3) = ipct
 	DO i = 1, 2
	    DO ii = 1, 2
		IF ( ipos (ii) .gt. ipos (ii+1) ) THEN
		    isave = ipos (ii)
		    ipos (ii) = ipos (ii+1)
		    ipos (ii+1) = isave
		END IF
	    END DO
	END DO
	DO i = 1, 3
	    IF ( ipos (i) .eq. ihat .and. ihat .ne. 0 ) ihat = i
	    IF ( ipos (i) .eq. iaat .and. iaat .ne. 0 ) iaat = i
	    IF ( ipos (i) .eq. ipct .and. ipct .ne. 0 ) ipct = i
	END DO
	ipos (4) = len + 1
C
C*	Check for override on the date/time stamp.
C
	IF ( ihat .ne. 0 ) THEN
	    istrt = ipos (ihat) + 1
	    istop = ipos (ihat+1) - 1
	    flgdtm (1)  = gdnbuf ( istrt:istop )
	    flgdtm (2) = ' '
	ELSE
C
C*	    Add century to YYMMDD.
C
	    flgdtm (1) = gdttm (1)
	    flgdtm (2) = ' '
	    islsh = INDEX ( gdttm (1), '/' )
	    IF ( islsh .lt. 9 ) THEN
		CALL ST_NUMB ( gdttm (1)(1:2), iyr, ier )
		IF ( iyr .lt. 70 ) THEN
		    flgdtm (1) = '20' // flgdtm (1)
		ELSE
		    flgdtm (1) = '19' // flgdtm (1)
		END IF
	    END IF
	END IF
C
C*	Check for override on the level.
C
	IF ( iaat .ne. 0 ) THEN
	    istrt = ipos (iaat) + 1
	    istop = ipos (iaat+1) - 1
	    olevl = gdnbuf ( istrt:istop )
	ELSE
	    olevl = ' '
	END IF
	IF ( ipct .ne. 0 ) THEN
	    istrt = ipos (ipct) + 1
	    istop = ipos (ipct+1) - 1
	    ovcrd = gdnbuf ( istrt:istop )
	ELSE
	    ovcrd = ' '
	END IF
	min = 9999
	DO i = 1, 3
	    IF ( ipos (i) .ne. 0 .and. ipos (i) .lt. min ) THEN
		min = ipos (i)
	    END IF
	END DO
	IF ( min .eq. 9999 ) THEN
	    istop = len
	ELSE
	    istop = min - 1
	END IF
	IF  ( gdnbuf .ne. ' ' )  THEN
	    oparm = gdnbuf (1:istop)
	  ELSE
	    oparm = ' ' 
	END IF
C
C*	Next, set the OCTET 4, 5, 6, and 26 values.
C
	CALL ST_ILST ( vercen, '/', 0, 4, ipos, num, ier )
	IF ( ipos (1) .ne. 0 ) THEN
	    noptv = ipos (1)
	ELSE
	    noptv = 2
	END IF
	IF ( ipos (2) .ne. 0 ) THEN
	    idoc = ipos (2)
	ELSE
	    idoc = 7
	END IF
	IF ( ipos (3) .ne. 0 ) THEN
	    idngp = ipos (3)
	ELSE
	    idngp = 0
	END IF
	IF ( ipos (4) .ne. 0 ) THEN
	    idosc = ipos (4)
	ELSE
	    idosc = 5
	END IF
C
C*	Translate vertical coordinate number as character string.
C
	IF ( ivcord .eq. 0 ) THEN
	    gvcord = 'NONE'
	ELSE IF ( ivcord .eq. 1 ) THEN
	    gvcord = 'PRES'
	ELSE IF ( ivcord .eq. 2 ) THEN
	    gvcord = 'THTA'
	ELSE IF ( ivcord .eq. 3 ) THEN
	    gvcord = 'HGHT'
	ELSE IF ( ivcord .eq. 4 ) THEN
	    gvcord = 'SGMA'
	ELSE IF ( ivcord .eq. 5 ) THEN
	    gvcord = 'DPTH'
	ELSE
	    gvcord = 'NULL'
	END IF
C
C*	Build the PDS now.
C
	CALL PDS_MAKE ( noptv, idoc, idngp, idosc, rnvblk,
     +			gparm, oparm, gvcord, ovcrd, level,
     +			olevl, havbms, ip10sc,
     +			lasttm, flgdtm, gbtbls, igpds,
     +			nbpds, cpds, ipos, iret )
C
C*	Convert day, hour, and minute in IPOS to characters.
C
	CALL ST_INCH ( ipos (1), c2d, ier )
	IF ( ipos (1) .lt. 10 ) THEN
	    cdum = '0' // c2d (1:1)
	ELSE
	    cdum = c2d
	END IF
	cdd = cdum
	CALL ST_INCH ( ipos (2), c2d, ier )
	IF ( ipos (2) .lt. 10 ) THEN
	    cdum = '0' // c2d (1:1)
	ELSE
	    cdum = c2d
	END IF
	chhmm (1:2) = cdum
	CALL ST_INCH ( ipos (3), c2d, ier )
	IF ( ipos (3) .lt. 10 ) THEN
	    cdum = '0' // c2d (1:1)
	ELSE
	    cdum = c2d
	END IF
	chhmm (3:4) = cdum
C*
	RETURN
	END
