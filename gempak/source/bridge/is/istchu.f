	SUBROUTINE IS_TCHU ( report, lenr, ibegin, iptrin, phenom, 
     +			     stidnt, icorr, lunf, origin, iptout, 
     +			     clat, clon, iret )
C************************************************************************
C* IS_TCHU 								*
C*									*
C* This subroutine decodes the fields which may follow a report of a    *
C* tropical cyclone, hurricane, typhoon, tropical storm, or tropical	*
C* depression. 								*
C*                                                                      *
C* IS_TCHU ( REPORT, LENR, IBEGIN, IPTRIN, PHENOM, STIDNT, ICORR, LUNF, *
C* 	     ORIGIN, IPTOUT, CLAT, CLON, IRET )                         *
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		International sigmet report     *
C*	LENR		INTEGER		Length of bulletin              *
C*	IBEGIN		INTEGER		Pointer to location before phen.*
C*	IPTRIN		INTEGER		Offset to pointer               *
C*	PHENOM		CHAR*		Type of phenomenon              *
C*	STIDNT		CHAR*		Time and message id string      *
C*	ICORR		INTEGER		Correction flag                 *
C*	LUNF		INTEGER		ASCII output file number        *
C*	ORIGIN		CHAR*		Originating station id		*
C*									*
C* Output parameters:							*
C*	IPTOUT		INTEGER		Pointer after last field decoded*
C*	CLAT		REAL		Latitude of storm center        *
C*	CLON		REAL		Longitude of storm center       *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-10 = no storm center found     *
C*									*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	12/03	Merged IS_HURC and IS_TC; fixed missing	*
C*				pressure handling.			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	report, phenom, stidnt, origin
C*	
	CHARACTER	stname*20, ptime*4
	CHARACTER	mxwnd*4, starr (3)*20, press*6
	INTEGER		iprswnd (2)
C------------------------------------------------------------------------
	iret  = 0
C
C*	Get the name of the storm.
C
	ibeg = ibegin + iptrin - 1
	CALL ST_LDSP ( report ( ibeg:ibeg + 19 ), stname, len, ier )
	iend = INDEX ( stname, ' ' ) - 1
	IF ( iend .lt. 0 ) iend = len
	stname = stname ( :iend )
	CALL ST_ALNM (stname (1:1), ityp, ier)
	IF ( ityp .ne. 2 ) stname = ' '
C
C*	Get the location (center) of the storm.
C*	(Account for center not close to name by skipping over 'NEAR'.)
C
	inext = ibeg + iend + 1
	inear = INDEX ( report ( inext:inext + 35 ), 'NEAR' )
	IF ( inear .eq. 0 ) THEN
	    inear = INDEX ( report ( inext:inext + 35 ), ' NR ' )
	    ioffst = 2
	  ELSE
	    ioffst = 3
	END IF
	IF ( inear .ne. 0 ) inear = inear + ioffst
	ibeg   = ibeg + iend + inear
	istart = ibeg
	lens   = lenr - ibeg + 1
	CALL IS_CENT ( report ( ibeg:lenr ), lens, clat, clon, iptr,
     +		       iret )
	IF ( iret .lt. 0 ) THEN
	    iptout = iptrin
	    RETURN
	  ELSE
	    iptout = ibeg + iptr - 1
	END IF
C
C*	Get the time.
C
	ibeg = iptout
	lens = lenr - ibeg + 1
	CALL IS_TIME ( report ( ibeg:lenr ), lens, ptime, iptr, iret )
	IF ( iret .lt. 0 ) THEN
	    iptr = 1
	  ELSE
	    iptout = ibeg + iptr - 1
	END IF
C
C*	Get the direction and speed of movement.
C
	ibeg = ibeg + iptr - 1
	lens = lenr - ibeg + 1
	CALL IS_MOV ( report ( ibeg:lenr ), lens, 40, idir, ispd, iptr, 
     +		      iret )
	IF ( iret .ge. 0 ) THEN
	    iptout = ibeg + iptr - 1
	  ELSE
C
C*	    Look for movement earlier in the string.
C
	    lens = lenr - istart + 1
	    CALL IS_MOV ( report ( istart:lenr ), lens, 40, idir, ispd,
     +			  iptr, iret )
	END IF
C
C*	Get the central pressure of the storm
C
	ipres = INDEX ( report ( inext:inext + 25 ), 'HPA' )
	IF ( ipres .eq. 0 ) THEN
	    iprswnd(1) = IMISSD
	  ELSE
	    CALL ST_CLSL (report (inext:inext+ipres-2), ' ', ' ', 3,
     +			starr, inum, ier )
	    press = starr(inum) (1:6)
	    CALL ST_RMBL ( press, press, lenp, ier )
	    CALL ST_INTG ( press(:lenp), iprswnd(1), ier )
	END IF
C
C*	Get the max winds
C
	imax = INDEX ( report, 'MAX WINDS' )
	IF  ( imax .ne. 0 ) THEN
	    imax = imax + 9
	  ELSE
	    imax = INDEX ( report, 'MAX WIND' )
	    IF  ( imax .ne. 0 )  imax = imax + 8
	END IF
	IF ( imax .ne. 0 ) THEN
	    ikt2 = INDEX ( report (imax: ), 'KT' ) - 1
	    IF ( ikt2 .gt. 0 ) THEN
		CALL ST_ALNM ( report (imax+ikt2:imax+ikt2), ityp, iret)
		IF ( ityp .eq. 2 ) ikt2 = ikt2 - 1
		mxwnd =  report ( imax:imax + ikt2)
		CALL ST_RMBL ( mxwnd, mxwnd, lenm, iret )
		CALL ST_INTG ( mxwnd(:lenm), iprswnd(2), iret )
	      ELSE
		iprswnd(2) = IMISSD
	    END IF
	  ELSE
	    iprswnd(2) = IMISSD
	END IF
C
C*	Write out the tropical disturbance data.
C
	IF ( iprswnd (1) .eq. IMISSD .and.
     +			iprswnd (2) .eq. IMISSD )  THEN
	    iflflg = IMISSD
	  ELSE
	    iflflg = 10
	END IF
	CALL IS_OUT ( lunf, phenom, stidnt, origin, icorr,
     +		      iflflg, iprswnd, idir, ispd, stname, IMISSD, 
     +		      clat, clon, 1, iret )
C*
	RETURN
	END
