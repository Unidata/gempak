	SUBROUTINE BFRRDD ( lunin,
     +			    catprm, ncats, ncatvl, catvls,
     +			    nprmsn, nprmsf, sncfac, sfcfac,
     +			    snctrm, sfctrm, parmsn, parmsf,
     +			    bprmsn, bprmsf, irepsf, nbsn, nbsf,
     +			    istrtf, dattim, stid, istnm, slat, slon,
     +			    selv, sndata, nz, sfdata, ns, ihhmm, iret )
C************************************************************************
C* BFRRDD								*
C*									*
C* This subroutine reads the next BUFR message from a Jack Woollen	*
C* BUFR file using Jack's routines called via the JB library.		*
C*									*
C* The surface parameter array contains data location parameters	*
C* having the following GEMPAK names and units.  Those parameters so	*
C* indicated in the table below are required.  Set up the conversion	*
C* table to convert data into the units shown in the table.  These	*
C* parameters must be placed first in the table.  They may be in any	*
C* order.								*
C*									*
C* GEMPAK name	Description	Units/type  Required?	Returned in	*
C*									*
C*   STID       Identifier       CHARACTER     NO         STID		*
C*   BLOK       Block number     INTEGER       NO           +		*
C*   STNM       Identifier	 INTEGER       NO	  ISTNM		*
C    SLAT	Latitude	 Degrees       YES	  SLAT		*
C*   SLON	Longitude	 Degrees       YES	  SLON		*
C*   SELV	Elevation	 Meters	       NO	  SELV		*
C*   YYYY       Year		 INTEGER       NO         DATTIM	*
C*   MMMM       Month		 INTEGER       NO	  DATTIM	*
C*   DDDD       Day		 INTEGER       NO	  DATTIM	*
C*   HHHH       Hour		 INTEGER       NO	  DATTIM	*
C*   NNNN	Minute		 INTEGER       NO	  DATTIM	*
C*   TOFF       Offset time	 Seconds       NO         IHHMM*	*
C*   TADD       Offset time	 Seconds       NO         DATTIM**	*
C*									*
C*	+ BLOK is combined with STNM to make a station number returned  *
C*	  in STNM.							*
C*	* TOFF is used in computing IHHMM.				*
C*     ** TADD is used in computing actual station time, DATTIM.	*
C*									*
C* If YYYY, MMMM, DDDD, HHHH, NNNN are given, DATTIM is computed from	*
C* them, and IHHMM = 0.  If NNNN is provided, the time may be rounded	*
C* to the nearest fraction of an hour as determined by the input for	*
C* the factor associated with NNNN.  For example, if SFCFAC for NNNN is	*
C* 30, the data is rounded to the nearest half hour.  The value in	*
C* SFCTRM is used as an offset in doing the rounding.  The rounded	*
C* minute value is computed as follows:					*
C*									*
C*  rounded_minute = sfcfac * NINT ( ( nnnn + sfctrm ) / sfcfac )	*
C*  IF ( rounded_minute .ge. 60 ) rounded_minute = 00			*
C*  IF ( rounded_minute .lt. 00 ) rounded_minute = 00			*
C*									*
C* If the SFCFAC = 1. for NNNN, no rounding is done.			*
C*									*
C* IF BLOK is present, STNM must be present in order to make a station  *
C* number by adding the block number to the station number.  It is	*
C* the user's responsible to specify the correct factor for BLOK to	*
C* yield the correct number of digits for the station number.		*
C*									*
C* The station number will default to a counter value if no STNM is	*
C* provided.  SELV and TOFF will both default to zero.			*
C*									*
C* The observation time, IHHMM, is the hour and minute of the time	*
C* stored in the BUFR file plus the offset value, TOFF.			*
C*									*
C* NS values of single level data will be returned in the array SFDATA. *
C* NS = NPRMSF - ISTRTF + 1.						*
C*									*
C* NPRMSN * NZ values of sounding data will be returned in SNDATA if	*
C* NPRMSN > 0.  SNDATA contains all of the values on level 1 followed	*
C* by all of the values on level 2, and so on.				* 
C*									*
C* CATPRM, NCATS, NCATVL, CATVLS contain the screening information	*
C* to allow values in the BUFR message to select the data.		*
C*									*
C*									*
C* BFRRDD  ( LUNIN, CATPRM, NCATS, NCATVL, CATVLS,			*
C*	     NPRMSN, NPRMSF, SNCFAC, SFCFAC, SNCTRM, SFCTRM,		*
C*	     PARMSN, PARMSF, BPRMSN, BPRMSF, IREPSF, NBSN, NBSF,	*
C*	     ISTRTF, DATTIM, STID, ISTNM, SLAT, SLON, SELV, SNDATA, NZ,	*
C*	     SFDATA, NS, IHHMM, IRET )					*
C*									*
C* Input parameters:							*
C*	LUNIN		INTEGER		BUFR file unit number		*
C*	CATPRM		CHAR*		Blank-separated mnemonic list	*
C*	NCATS		INTEGER		Number in the list		*
C*	NCATVL (NCATS)	INTEGER		# of values for each mnemonic	*
C*	CATVLS (*)	REAL		Values				*
C*	NPRMSN		INTEGER		Number of sounding parms	*
C*	NPRMSF		INTEGER		Number of surface parms		*
C*	SNCFAC (NPRMSN) REAL		Conversion factors for snd data	*
C*	SFCFAC (NPRMSF) REAL		Conversion factors for sfc data *
C*	SNCTRM (NPRMSN) REAL		Conversion terms for snd data	*
C*	SFCTRM (NPRMSF) REAL		Conversion terms for sfc data	*
C*	PARMSN (NPRMSN) CHAR*4		Sounding parm list		*
C*	PARMSF (NPRMSF) CHAR*4		Surface parm list		*
C*	BPRMSN (NBSN)	CHAR*80		BUFR sounding parm list		*
C*	BPRMSF (NBSF)	CHAR*80		BUFR surface parm list		*
C*	IREPSF (NBSF)	INTEGER		Replication #'s for sfc parms	*
C*	NBSN		INTEGER		# of 80-char sounding prm lists *
C*	NBSF		INTEGER		# of 80-char surface prm lists	*
C*	ISTRTF		INTEGER		Index of 1st real sfc parm	*
C*									*
C* Output parameters:							*
C*	DATTIM		CHAR*		Date/time			*
C*	STID		CHAR*		Station ID			*
C*	ISTNM		INTEGER		Station number			*
C*	SLAT		REAL		Station latitude		*
C*	SLON		REAL		Station longitude		*
C*	SELV		REAL		Station elevation		*
C*	SNDATA (*)	REAL		Array of sounding data		*
C*	NZ		INTEGER		Number of levels in sounding	*
C*	SFDATA (*)	REAL		Array of sfc (single-lvl) data	*
C*	NS		INTEGER		Number of sfc values		*
C*	IHHMM		INTEGER		Observation time		*
C*	IRET		INTEGER		Return code			*
C*					 +3 = Message rejected by screen*
C*					 +1 = End of BUFR file		*
C*					  0 = normal			*
C*					 -3 = Missing station lat/lon	*
C*					-12 = No station location info	*
C*					-13 = Read error on BUFR file	*
C*					-14 = Sfc data read failed	*
C*					-15 = Not enough replicated data*
C*					-16 = Snd data read failed	*
C*					-17 = # of snd levels varies	*
C*					-20 = SFC parm count mismatch	*
C*					-21 = SND parm count mismatch	*
C*					-27 = Data overflow		*
C**									*
C* Log:									*
C* K. Brill/EMC		 2/97						*
C* K. Brill/EMC		 2/97	Changes for character station ID & TADD	*
C* K. Brill/EMC		 3/97	Changes for rounded minute		*
C* K. Brill/EMC		 7/98	Convert STID to ISTNM if valid;		*
C*				Call BFRCHK for screening parameters	*
C* K. Brill/HPC		 3/01	Use LVLFLG to screen levels		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	PARAMETER	( RMISSB = 1.0E11, EPSB = .01 )
C*
	CHARACTER*(*)	parmsn (*), parmsf (*), catprm, stid, dattim
	CHARACTER*80	bprmsn (*), bprmsf (*)
	INTEGER		irepsf (*), ncatvl (*)
	REAL		sncfac (*), sfcfac (*), snctrm (*), sfctrm (*)
	REAL		sndata (*), sfdata (*), catvls (*)
C*
	REAL*8		snbufr ( MMPARM * LLMXLV ), sfbufr ( 2*MMPARM )
	INTEGER		ipcnts (MMPARM), itm (5)
	REAL*8		rstid
	CHARACTER*8	cstid
	EQUIVALENCE	( rstid, cstid )
	CHARACTER*20	dtmact
	LOGICAL		lvlflg (LLMXLV), msgok
C*
	SAVE		istcnt
C*
	DATA		istcnt /0/
	INCLUDE		'ERMISS.FNC'
C----------------------------------------------------------------------
	iret = 0
C
C*	Check for required BUFR location information in PARMSF.
C
	jblok = 0
	jstid = 0
	jstnm = 0
	jslat = 0
	jslon = 0
	jselv = 0
	jyyyy = 0
	jmmmm = 0
	jdddd = 0
	jhhhh = 0
	jnnnn = 0
	jtoff = 0
	jtadd = 0
	DO i = 1, istrtf - 1
	    IF ( parmsf (i) .eq. 'BLOK' ) jblok = i
	    IF ( parmsf (i) .eq. 'STID' ) jstid = i
	    IF ( parmsf (i) .eq. 'STNM' ) jstnm = i
	    IF ( parmsf (i) .eq. 'SLAT' ) jslat = i
	    IF ( parmsf (i) .eq. 'SLON' ) jslon = i
	    IF ( parmsf (i) .eq. 'SELV' ) jselv = i
	    IF ( parmsf (i) .eq. 'YYYY' ) jyyyy = i
	    IF ( parmsf (i) .eq. 'MMMM' ) jmmmm = i
	    IF ( parmsf (i) .eq. 'DDDD' ) jdddd = i
	    IF ( parmsf (i) .eq. 'HHHH' ) jhhhh = i
	    IF ( parmsf (i) .eq. 'NNNN' ) jnnnn = i
	    IF ( parmsf (i) .eq. 'TOFF' ) jtoff = i
	    IF ( parmsf (i) .eq. 'TADD' ) THEN
		jtoff = i
		jtadd = i
	    END IF
	END DO
	IF ( jslat .eq. 0 .or. jslon .eq. 0 ) THEN
	    iret = -12
	    RETURN
	END IF
C
C*	Read in the next BUFR message.
C
	CALL JB_NEXT ( lunin, iymdh, ier )
	IF ( ier .eq. -1 ) THEN
	    iret = +1
	    RETURN
	ELSE IF ( ier .ne. 0 ) THEN
	    iret = -13
	    RETURN
	END IF
	CALL BFRCHK ( lunin, catprm, ncats, ncatvl, catvls, msgok,
     +		      lvlflg, iret )
	IF ( .not. msgok .or. iret .lt. 0 ) THEN
	    iret = +3
	    RETURN
	END IF
C
C*	Read surface parameters.
C
	indx = 1
	mxdat = MMPARM
	DO i = 1, nbsf
	    mxdat = mxdat - indx + 1
	    IF ( mxdat .le. 0 ) THEN
		iret = -27
		RETURN
	    END IF
	    CALL JB_READ ( lunin, bprmsf (i), mxdat, sfbufr (indx),
     +			   np, nl, ier )
	    IF ( ier .ne. 0 ) THEN
		iret = -14
		RETURN
	    END IF
	    IF ( irepsf (i) .gt. 0 ) THEN
C
C*		IREPSF is the number of replications of the value; so,
C*		IREPSF + 1 values are needed.
C
		IF ( irepsf (i) .ge. nl ) THEN
		    iret = -15
		    RETURN
		END IF
		np = irepsf (i) + 1
	    END IF
	    DO j = indx, indx + np - 1
		fsfb = sfbufr (j)
		IF ( ABS ( fsfb - RMISSB ) .gt. EPSB .and.
     +		     j .ne. jnnnn ) THEN
		    sfbufr (j) = sfcfac (j) * sfbufr (j) + sfctrm (j)
		ELSE IF ( j .ne. jnnnn ) THEN
		    sfbufr (j) = RMISSD
		END IF
	    END DO
	    indx = indx + np
	END DO
	iendf = indx - 1
	IF ( nprmsf .ne. iendf ) THEN
	    iret = -20
	    RETURN
	END IF
C
C*	Load station header information.
C
	qstid = 0.
	IF ( jstid .ne. 0 ) qstid = sfbufr (jstid)
	IF ( jstid .ne. 0 .and. .not. ERMISS ( qstid ) ) THEN
	    rstid = sfbufr (jstid)
	    stid = cstid
	ELSE
	    stid = ' '
	END IF
	rstnm = 0.
	IF ( jstnm .ne. 0 ) rstnm = sfbufr (jstnm)
	IF ( jstnm .ne. 0 .and. .not. ERMISS ( rstnm ) ) THEN
	    istnm = sfbufr (jstnm)
	ELSE
	    istcnt = istcnt + 1
	    istnm = istcnt
	END IF
	rblok = 0.
	IF ( jblok .ne. 0 ) rblok = sfbufr (jblok)
	IF ( jblok .ne. 0 .and. .not. ERMISS ( rblok ) ) THEN
	    iblok = sfbufr (jblok)
	    IF ( jstnm .ne. 0 ) istnm = iblok + istnm
	END IF
	slat = sfbufr ( jslat )
	slon = sfbufr ( jslon )
	IF ( ERMISS (slat) .or. ERMISS (slon) ) THEN
	    iret = -3
	    RETURN
	END IF
	rselv = 0.
	IF ( jselv .ne. 0 ) rselv = sfbufr (jselv)
	IF ( jselv .ne. 0 .and. .not. ERMISS ( rselv ) ) THEN
	    selv = sfbufr (jselv)
	ELSE
	    selv = 0.
	END IF
	rtoff = 0.
	IF ( jtoff .ne. 0 ) rtoff = sfbufr (jtoff)
	IF ( jtoff .ne. 0 .and. .not. ERMISS ( rtoff ) ) THEN
	    toff = sfbufr (jtoff)
	ELSE
	    toff = 0.
	END IF
	IF ( stid .eq. ' ' ) THEN
	    CALL ST_INCH ( istnm, stid, ier )
	ELSE
	    IF ( jstnm .eq. 0 ) istnm = IMISSD
	END IF
C
C*	If valid, use STID to get a station number.
C
	IF ( istnm .eq. IMISSD ) THEN
	    CALL ST_NUMB ( stid, isnumb, ier )
	    IF ( ier .eq. 0 ) THEN
		istnm = isnumb
	    END IF
	END IF
C*
	iyyyy = 0
	IF ( jyyyy .ne. 0 ) ryyyy = sfbufr (jyyyy)
	IF ( jyyyy .ne. 0 .and. .not. ERMISS ( ryyyy ) ) THEN
	    iyyyy = sfbufr (jyyyy)
	END IF
	immmm = 0
	IF ( jmmmm .ne. 0 ) rmmmm = sfbufr (jmmmm)
	IF ( jmmmm .ne. 0 .and. .not. ERMISS ( rmmmm ) ) THEN
	    immmm = sfbufr (jmmmm)
	END IF
	idddd = 0
	IF ( jdddd .ne. 0 ) rdddd = sfbufr (jdddd)
	IF ( jdddd .ne. 0 .and. .not. ERMISS ( rdddd ) ) THEN
	    idddd = sfbufr (jdddd)
	END IF
	ihhhh = 0
	IF ( jhhhh .ne. 0 ) rhhhh = sfbufr (jhhhh)
	IF ( jhhhh .ne. 0 .and. .not. ERMISS ( rhhhh ) ) THEN
	    ihhhh = sfbufr (jhhhh)
	END IF
	innnn = 0
	IF ( jnnnn .ne. 0 ) rnnnn = sfbufr (jnnnn)
	IF ( jnnnn .ne. 0 .and. .not. ERMISS ( rnnnn ) ) THEN
	    innnn = NINT ( sfbufr (jnnnn) )
	    IF ( sfcfac (jnnnn) .ne. 1. ) THEN
		ifac = NINT ( sfcfac (jnnnn) )
		itrm = NINT ( sfctrm (jnnnn) )
		innnn = ifac * NINT ( FLOAT ( innnn + itrm ) /
     +					FLOAT ( ifac ) )
		IF ( innnn .ge. 60 ) innnn = 0
		IF ( innnn .lt. 00 ) innnn = 0
	    END IF
	END IF
C
C*	Load surface/single-layer parameters.
C
	ns = 0
	IF ( nprmsf .ge. istrtf ) THEN
	    DO i = istrtf, iendf
		ns = ns + 1
		sfdata (ns) = sfbufr (i)
	    END DO
	END IF
C
C*	Set DATTIM.
C
	IF ( jyyyy .eq. 0 ) THEN
	    CALL BFRTIM ( iymdh, toff, dattim, dtmact, ihhmm, ier )
	    IF ( jtadd .ne. 0 ) THEN
		dattim = dtmact
		ihhmm = 0
	    END IF
	ELSE
	    itm (1) = iyyyy
	    itm (2) = immmm
	    itm (3) = idddd
	    itm (4) = ihhhh
	    itm (5) = innnn
	    CALL TI_ITOC ( itm, dattim, ier )
	    ihhmm = 0
	END IF
C
C*	Read sounding parameters.
C
	indx = 1
	mxdat = MMPARM * LLMXLV
	ipsum = 0
	mpstrt = 1
	DO i = 1, nbsn
	    mxdat = mxdat - indx + 1
	    IF ( mxdat .le. 0 ) THEN
		iret = -27
		RETURN
	    END IF
	    CALL JB_READ ( lunin, bprmsn (i), mxdat, snbufr (indx),
     +			   np, nl, ier )
	    IF ( ier .ne. 0 ) THEN
		iret = -16
		RETURN
	    END IF
	    IF ( indx .eq. 1 ) THEN
		nz = nl
	    ELSE
		IF ( nl .ne. nz ) THEN
		    iret = -17
		    RETURN
		END IF
	    END IF
	if ( stid .eq. '94659' ) then
C	    print *, nz, ' level flags:'
C	    print *, (lvlflg(ix),ix=1,nz)
	    i1 = indx
	    do k=1,nz
	       i2 = i1 + np - 1
C	       print *, 'lvl ',k,' ',(snbufr(ix),ix=i1,i2)
	       i1 = i1 + np
	    enddo
	endif
	    
	    DO k = 1, nz
		m = mpstrt
		DO j = indx, indx + np - 1
		    fsnb = snbufr (j)
		    IF ( ABS ( fsnb - RMISSB ) .gt. EPSB ) THEN
			snbufr (j) = sncfac (m) * snbufr (j)
     +					+ snctrm (m)
		    ELSE
			snbufr (j) = RMISSD
		    END IF
		    m = m + 1
		END DO
		indx = indx + np
	    END DO
	    ipcnts (i) = np
	    ipsum = ipsum + np
	    mpstrt = mpstrt + np
	END DO
	IF ( ipsum .ne. nprmsn ) THEN
	    iret = -21
	    RETURN
	END IF
C
C*	Load output array with all parameters on level one,
C*	followed by all on level two, etc.
C
	iout = 0
	nzo = 0
	DO k = 1, nz
	    IF ( lvlflg (k) ) THEN
		nzo = nzo + 1
		indx = ( k - 1 ) * ipcnts (1) + 1
		DO i = 1, nbsn
		    DO j = indx, indx + ipcnts (i) - 1
			iout = iout + 1
			sndata (iout) = snbufr (j)
		    END DO
		    IF ( i .ne. nbsn )
     +		      indx = ( nz - k + 1 ) * ipcnts (i) + indx +
     +			     ( k - 1 ) * ipcnts (i+1)
		END DO
	    END IF
	END DO
	nz = nzo
C*
	RETURN
	END
