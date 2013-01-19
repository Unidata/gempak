	SUBROUTINE IS_OUT ( lunf, phenom, stidnt, origin, icorr, iflflg, 
     +			    iflvl, idir, ispd, stname, irad, rlat, rlon,
     +			    npt, iret )
C************************************************************************
C* IS_OUT 								*
C*									*
C* This subroutine writes decoded international sigmet data to an ASCII *
C* file.                                                                *
C*                                                                      *
C* IS_OUT ( LUNF, PHENOM, STIDNT, ORIGIN, ICORR, IFLFLG, IFLVL, IDIR,   *
C*	    ISPD, STNAME, IRAD, RLAT, RLON, NPT, IRET )                 *
C*									*
C* Input parameters:							*
C*	LUNF		INTEGER		ASCII output file number	*
C*	PHENOM		CHAR*		Phenomenon                      *
C*	STIDNT		CHAR*		Time and message id string      *
C*      ORIGIN		CHAR*		Originating station id.		*
C*	ICORR		INTEGER		Correction flag                 *
C*	IFLFLG		INTEGER		Flight level flag               *
C*	IFLVL (*)	INTEGER		Flight level(s)                 *
C*	IDIR		INTEGER		Direction of movement (degrees) *
C*	ISPD		INTEGER		Speed (knots)                   *
C*	STNAME		CHAR*		Name of storm                   *
C*	IRAD		INTEGER		Radius if area is a circle      *
C*	RLAT (*)	REAL		Latitudes of points             *
C*	RLON (*)	REAL		Longitudes of points            *
C*	NPT		INTEGER		Number of points                *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	10/99	                                        *
C* D. Kidwell/NCEP	10/99	Removed cancel flag; add circle center  *
C* D. Kidwell/NCEP	11/99	Removed substring specifier for phenom  *
C* F. J. Yen/NCEP	03/00	Handled flight levels with no. char < 3 *
C* D. Kidwell/NCEP	 3/00	Checked for bad (missing) points        *
C* A. Hardy/GSC          7/00   Added press/max wind string display     *
C* A. Hardy/SAIC	 9/01	Added check for press/max wind 		*
C* A. Hardy/NCEP	 9/02	Added originating station id		*
C* F. J. Yen/NCEP	10/03	Allowed for 'SFC' & 'FZLVL' for FL lvl.	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	phenom, stidnt, stname, origin
	INTEGER		iflvl (*)
	REAL   		rlat (*), rlon (*)
C*
	CHARACTER	corr*1, ifl*9, ifl2*3, idr*3, isp*3, string*80
	CHARACTER	pres*4, wind*3
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
C*	Construct the header string.
C
	ifl = ' '
	idr = ' '
	isp = ' '
	lenfl = 1
	lendr = 1
	lensp = 1
	IF ( phenom .ne. 'CN' ) THEN
	    IF ( ( iflflg .ne. IMISSD ) .and. ( iflflg .ne. 10 ) ) THEN
	        IF ( iflvl (1) .ne. IMISSD ) THEN
		    lenfl = 3
		    IF ( iflvl (1) .eq. -5000 ) THEN
			ifl = 'SFC'
		      ELSE IF ( iflvl (1) .eq. -6000 ) THEN
			ifl = 'FZLVL'
		      ELSE
      		        CALL ST_INCH ( iflvl (1), ifl, ier )
		        IF ( iflvl (1) .lt. 10 ) THEN
		            ifl = '00' // ifl ( :1)
		          ELSE IF ( iflvl (1) .lt. 100 ) THEN
			    ifl = '0' // ifl ( :2)
		        END IF 
		    END IF
		    IF ( ( iflflg .eq. 2 ) .and. 
     +			 ( iflvl ( 2 ) .ne. IMISSD ) ) THEN
			CALL ST_INCH ( iflvl (2), ifl2, ier )
		        IF ( iflvl (2) .lt. 10 ) THEN
			    ifl2 = '00' // ifl2 ( :1)
		          ELSE IF ( iflvl (2) .lt. 100 ) THEN
			    ifl2 = '0' // ifl2 ( :2)
		        END IF 
			IF ( ifl .eq. 'FZLVL' ) THEN
		            ifl = ifl ( :5) // '-' // ifl2
			    lenfl = 9
			  ELSE
		            ifl = ifl ( :3) // '-' // ifl2
			    lenfl = 7
			END IF
		    END IF
		END IF
              ELSE IF ( iflflg .eq. 10 ) THEN
                IF ( iflvl(1) .ne. IMISSD ) THEN
      		    CALL ST_INCH ( iflvl (1), pres, ier )
                  ELSE
                    pres = ' '
                END IF
                IF ( iflvl(2) .ne. IMISSD ) THEN
      		    CALL ST_INCH ( iflvl (2), wind, ier )
                  ELSE
                    wind = ' '
                END IF
C
		CALL ST_LSTR ( pres, len1, ier )
		CALL ST_LSTR ( wind, len2, ier )
		ifl = pres ( :len1) // '-' // wind(:len2)
		CALL ST_LSTR ( ifl, lenfl, ier )
	    END IF
	    IF ( idir .ne. IMISSD ) THEN
		CALL ST_INCH ( idir, idr, ier )
		CALL ST_LSTR ( idr, lendr, ier )
	    END IF
	    IF ( ispd. ne. IMISSD ) THEN
		CALL ST_INCH ( ispd, isp, ier )
		CALL ST_LSTR ( isp, lensp, ier )
	    END IF 
	END IF
	CALL ST_INCH ( icorr, corr, ier )
	CALL ST_LSTR ( stidnt, lenstr, ier )
	CALL ST_LSTR ( stname, lennam, ier )
	CALL ST_LSTR ( origin,  lenloc, ier )
	IF ( lennam .lt.1 ) lennam = 1
	string = '|' // phenom // '|' // stidnt ( :lenstr ) //
     +		 '|' // origin ( :lenloc ) // 
     +		 '|' // ifl ( :lenfl ) // '|' // idr ( :lendr ) // 
     +		 '|' // isp ( :lensp ) // '|' // stname ( :lennam ) //
     +		 '|' // corr
C
	WRITE ( lunf, 10 ) string
   10	FORMAT ( A )
C
C*	Write out the points.
C
	igood = 0
	DO i = 1, npt
	    IF ( ERMISS ( rlat ( i ) ) .or. ERMISS ( rlon ( i ) ) ) THEN
	      ELSE
	        WRITE ( lunf, 20 ) rlat (i), rlon (i)
		igood = igood + 1
	    END IF
	END DO
	IF ( ( irad .ne. IMISSD )  .and. ( igood .eq. 1 ) ) THEN
	    rad = irad
	    WRITE ( lunf, 20 ) rad, RMISSD
	END IF
   20 	FORMAT ( 2F9.2 )
C*
	RETURN
	END
